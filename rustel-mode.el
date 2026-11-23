;;; rustel-mode.el --- Minimal Rustel/Strudel editing and live playback -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:
;; A small JavaScript-derived major mode for Rustel scores.  It starts
;; `rustel play FILE --watch --ui-events' and highlights source ranges for
;; events currently sounding.  Rustel must be on `exec-path'.
;;
;; This is a first experiment, not a complete client for rustel-studio or a
;; stable implementation of Rustel's currently hidden --ui-events protocol.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'js)
(require 'seq)
(require 'subr-x)

(defgroup rustel nil
  "Editing and live playback of Rustel/Strudel scores."
  :group 'languages)

(defcustom rustel-command "rustel"
  "Rustel executable used by `rustel-play'."
  :type 'string
  :group 'rustel)

(defcustom rustel-play-arguments nil
  "Extra arguments passed to `rustel play'.

For example, set this to a list containing `--output' and `silent' to run
the clock without opening an audio device."
  :type '(repeat string)
  :group 'rustel)

(defface rustel-active-face
  '((t :inherit highlight))
  "Face used for source ranges of notes that are sounding."
  :group 'rustel)

(defvar rustel--active-process nil
  "The one Rustel playback process shared by all Rustel buffers.")

(defvar-local rustel--process nil)
(defvar-local rustel--timer nil)
(defvar-local rustel--overlays nil)
(defvar-local rustel--source-bytes nil)
(defvar-local rustel--source-revision nil)
(defvar-local rustel--source-coding nil)

(defvar rustel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'rustel-play)
    (define-key map (kbd "C-c C-k") #'rustel-stop)
    map)
  "Keymap for `rustel-mode'.")

(defun rustel--select-utf8-coding ()
  "Select UTF-8 for buffers whose file coding is still undecided."
  (when (eq (coding-system-base (or buffer-file-coding-system 'undecided-unix))
            'undecided)
    (let ((eol (coding-system-eol-type buffer-file-coding-system)))
      (setq-local buffer-file-coding-system
                  (coding-system-change-eol-conversion
                   'utf-8-unix (if (integerp eol) eol 0))))))

(defun rustel--file-coding ()
  "Return the current buffer's UTF-8 file coding system, or refuse it."
  (let ((coding (or buffer-file-coding-system 'utf-8-unix)))
    (unless (memq (coding-system-base coding) '(utf-8 utf-8-with-signature))
      (user-error "Rustel scores must be UTF-8 (buffer coding is %s)" coding))
    coding))

(defun rustel--refresh-source-snapshot ()
  "Record the current buffer as UTF-8 bytes and its Rustel revision hash."
  (setq rustel--source-bytes nil
        rustel--source-revision nil
        rustel--source-coding nil)
  (when buffer-file-name
    (let* ((coding (rustel--file-coding))
           (text (buffer-substring-no-properties (point-min) (point-max)))
           (bytes (encode-coding-string text coding)))
      (setq rustel--source-bytes bytes
            rustel--source-revision (secure-hash 'sha256 bytes)
            rustel--source-coding coding))))

(defun rustel--byte-to-position (offset)
  "Translate Rustel's UTF-8 byte OFFSET into a buffer position."
  (when (and (integerp offset)
             (>= offset 0)
             rustel--source-bytes
             (<= offset (length rustel--source-bytes)))
    (condition-case nil
        (+ (point-min)
           (length
            (decode-coding-string
             (substring rustel--source-bytes 0 offset)
             rustel--source-coding)))
      (coding-system-error nil))))

(defun rustel--clear-overlays ()
  "Remove this buffer's live-note overlays."
  (mapc #'delete-overlay rustel--overlays)
  (setq rustel--overlays nil))

(defun rustel--after-change (&rest _ignored)
  "Do not leave live markers attached to text that has since changed."
  (rustel--clear-overlays))

(defun rustel--after-save ()
  "Refresh the source revision after a score is saved."
  (rustel--refresh-source-snapshot)
  (rustel--clear-overlays))

(defun rustel--json-object (line)
  "Parse one LINE of Rustel NDJSON as an alist, or return nil."
  (unless (string-empty-p line)
    (condition-case nil
        (json-parse-string line
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object :false)
      (json-parse-error nil))))

(defun rustel--handle-layout (process layout)
  "Accept a new installed-score LAYOUT from PROCESS."
  (let ((generation (alist-get 'generation layout))
        (revision (alist-get 'source_revision layout))
        (buffer (process-get process 'rustel-buffer)))
    (when (and (buffer-live-p buffer)
               (integerp generation)
               (stringp revision))
      (with-current-buffer buffer
        (rustel--refresh-source-snapshot)
        (process-put process 'rustel-generation generation)
        (process-put process 'rustel-revision revision)
        (process-put process 'rustel-events nil)
        (process-put process 'rustel-offset nil)
        (process-put process 'rustel-device-time nil)
        (rustel--clear-overlays)
        (unless (equal revision rustel--source-revision)
          (message "Rustel installed a different source revision; live highlighting is paused"))))))

(defun rustel--event-entry (event offset revision)
  "Make an editor event from EVENT, clock OFFSET and source REVISION."
  (let ((target (alist-get 'target_time event))
        (duration (alist-get 'duration_seconds event))
        (context (alist-get 'context event)))
    (when (and (numberp target)
               (numberp duration)
               (> duration 0)
               (listp context))
      (list :start (+ target offset)
            :end (+ target offset duration)
            :revision revision
            :context context))))

(defun rustel--handle-events (process batch)
  "Accept scheduled onset BATCH records from PROCESS."
  (let* ((device-time (alist-get 'device_time batch))
         (generation (alist-get 'generation batch))
         (revision (alist-get 'source_revision batch))
         (now (float-time))
         (old-device-time (process-get process 'rustel-device-time))
         (old-offset (process-get process 'rustel-offset))
         (current-generation (process-get process 'rustel-generation))
         (current-revision (process-get process 'rustel-revision)))
    (when (and (numberp device-time)
               (numberp generation)
               (equal generation current-generation)
               (equal revision current-revision))
      (when (and old-device-time (< device-time old-device-time))
        ;; The audio device clock restarted or changed streams.
        (setq old-offset nil)
        (process-put process 'rustel-events nil))
      ;; Arrival can be late. Keep the smallest observed host/device offset,
      ;; which gives a steadier estimate of when queued notes reach the device.
      (let* ((candidate (- now device-time))
             (offset (if old-offset (min old-offset candidate) candidate))
             (events (process-get process 'rustel-events)))
        (when (and old-offset (/= offset old-offset))
          (let ((delta (- offset old-offset)))
            (setq events
                  (mapcar (lambda (entry)
                            (plist-put
                             (plist-put entry :start (+ (plist-get entry :start) delta))
                             :end (+ (plist-get entry :end) delta)))
                          events))))
        (dolist (event (alist-get 'events batch))
          (let ((entry (rustel--event-entry event offset revision)))
            (when entry (push entry events))))
        (setq events
              (seq-take
               (seq-filter (lambda (entry)
                             (> (plist-get entry :end) (- now 1.0)))
                           events)
               1024))
        (process-put process 'rustel-events events)
        (process-put process 'rustel-offset offset)
        (process-put process 'rustel-device-time device-time)))))

(defun rustel--handle-record (process record)
  "Dispatch one decoded UI RECORD from PROCESS."
  (when-let ((layout (alist-get 'ui_layout record)))
    (rustel--handle-layout process layout))
  (when-let ((batch (alist-get 'ui_events record)))
    (rustel--handle-events process batch)))

(defun rustel--stdout-filter (process chunk)
  "Consume Rustel's newline-delimited UI protocol from PROCESS."
  (let* ((pending (concat (or (process-get process 'rustel-pending) "") chunk))
         (start 0)
         newline)
    (while (setq newline (string-match "\n" pending start))
      (let* ((line (substring pending start newline))
             (line (if (string-suffix-p "\r" line)
                       (substring line 0 -1)
                     line))
             (record (rustel--json-object line)))
        (when record (rustel--handle-record process record)))
      (setq start (1+ newline)))
    (setq pending (substring pending start))
    (if (> (length pending) (* 4 1024 1024))
        (progn
          (process-put process 'rustel-pending "")
          (display-warning 'rustel "Discarded an oversized UI protocol line" :warning))
      (process-put process 'rustel-pending pending))))

(defun rustel--stderr-filter (stderr chunk)
  "Keep Rustel diagnostics in a buffer and show structured live errors."
  (let ((buffer (process-get stderr 'rustel-stderr-buffer))
        (pending (concat (or (process-get stderr 'rustel-pending) "") chunk))
        (start 0)
        newline)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert chunk))))
    (while (setq newline (string-match "\n" pending start))
      (let* ((line (substring pending start newline))
             (record (rustel--json-object line))
             (live-error (and record (alist-get 'live_error record)))
             (message-text (and live-error (alist-get 'message live-error)))
             (parent (process-get stderr 'rustel-parent)))
        (when (stringp message-text)
          (unless (and (processp parent)
                       (equal message-text (process-get parent 'rustel-last-warning)))
            (when (processp parent)
              (process-put parent 'rustel-last-warning message-text))
            (display-warning 'rustel message-text :warning))))
      (setq start (1+ newline)))
    (process-put stderr 'rustel-pending (substring pending start))))

(defun rustel--refresh-overlays (buffer)
  "Refresh active-note overlays in BUFFER from its playback process."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (rustel--clear-overlays)
      (let ((process rustel--process)
            (now (float-time)))
        (when (and (process-live-p process)
                   (not (buffer-modified-p))
                   rustel--source-revision
                   (equal rustel--source-revision
                          (process-get process 'rustel-revision)))
          (dolist (event (process-get process 'rustel-events))
            (when (and (equal (plist-get event :revision) rustel--source-revision)
                       (<= (plist-get event :start) now)
                       (> (plist-get event :end) now))
              (dolist (range (plist-get event :context))
                (when (and (listp range) (= (length range) 2))
                  (let ((from (rustel--byte-to-position (nth 0 range)))
                        (to (rustel--byte-to-position (nth 1 range))))
                    (when (and from to (< from to))
                      (let ((overlay (make-overlay from to nil t nil)))
                        (overlay-put overlay 'face 'rustel-active-face)
                        (overlay-put overlay 'priority 120)
                        (overlay-put overlay 'help-echo "Rustel: note sounding")
                        (push overlay rustel--overlays)))))))))))))

(defun rustel--stop-timer (buffer)
  "Stop BUFFER's UI refresh timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp rustel--timer)
        (cancel-timer rustel--timer)
        (setq rustel--timer nil)))))

(defun rustel--sentinel (process _event)
  "Clean up after Rustel PROCESS exits."
  (when (memq (process-status process) '(exit signal failed))
    (let* ((buffer (process-get process 'rustel-buffer))
           (stderr-buffer (process-get process 'rustel-stderr-buffer))
           (expected (process-get process 'rustel-expected-stop))
           (bad-exit (not (and (eq (process-status process) 'exit)
                               (zerop (process-exit-status process))))))
      (when (eq process rustel--active-process)
        (setq rustel--active-process nil))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq rustel--process process)
            (setq rustel--process nil)
            (rustel--stop-timer buffer)
            (rustel--clear-overlays))))
      (when-let ((stderr (process-get process 'rustel-stderr-process)))
        (when (process-live-p stderr) (delete-process stderr)))
      (when (and bad-exit (not expected) (buffer-live-p stderr-buffer))
        (display-buffer stderr-buffer)))))

(defun rustel--buffer-killed ()
  "Stop playback when its score buffer is killed."
  (when (and (process-live-p rustel--process)
             (not (process-get rustel--process 'rustel-expected-stop)))
    (process-put rustel--process 'rustel-expected-stop t)
    (interrupt-process rustel--process))
  (rustel--stop-timer (current-buffer))
  (rustel--clear-overlays))

;;;###autoload
(defun rustel-play ()
  "Save this score and start or update Rustel playback."
  (interactive)
  (unless (and buffer-file-name (memq major-mode '(rustel-mode)))
    (user-error "Open a saved Rustel score buffer first"))
  (rustel--file-coding)
  (save-buffer)
  (rustel--refresh-source-snapshot)
  (unless rustel--source-revision
    (user-error "Could not determine the score's UTF-8 source revision"))
  (cond
   ((process-live-p rustel--process)
    (message "Saved; Rustel will install the changed score when its watcher notices it"))
   ((process-live-p rustel--active-process)
    (user-error "Rustel is already playing another buffer; stop it first"))
   ((not (executable-find rustel-command))
    (user-error "Cannot find Rustel executable: %s" rustel-command))
   (t
    (let* ((buffer (current-buffer))
           (stderr-buffer (get-buffer-create
                           (format "*Rustel stderr: %s*" (file-name-nondirectory buffer-file-name))))
           (stderr (make-pipe-process
                    :name (format "rustel-stderr-%d" (emacs-pid))
                    :buffer nil
                    :coding 'utf-8-unix
                    :noquery t
                    :filter #'rustel--stderr-filter))
           process)
      (with-current-buffer stderr-buffer
        (let ((inhibit-read-only t)) (erase-buffer)))
      (process-put stderr 'rustel-stderr-buffer stderr-buffer)
      (setq process
            (make-process
             :name (format "rustel-%s" (file-name-base buffer-file-name))
             :buffer nil
             :command (append (list rustel-command "play" buffer-file-name
                                    "--watch" "--ui-events")
                              rustel-play-arguments)
             :coding '(utf-8-unix . utf-8-unix)
             :connection-type 'pipe
             :noquery t
             :stderr stderr
             :filter #'rustel--stdout-filter
             :sentinel #'rustel--sentinel))
      (process-put process 'rustel-buffer buffer)
      (process-put process 'rustel-pending "")
      (process-put process 'rustel-events nil)
      (process-put process 'rustel-stderr-buffer stderr-buffer)
      (process-put process 'rustel-stderr-process stderr)
      (process-put stderr 'rustel-parent process)
      (setq rustel--process process
            rustel--active-process process
            rustel--timer (run-at-time 0 0.033 #'rustel--refresh-overlays buffer))
      (message "Rustel started: %s" (file-name-nondirectory buffer-file-name))))))

;;;###autoload
(defun rustel-stop ()
  "Send an interrupt to Rustel; repeat during the tail to cut it short."
  (interactive)
  (unless (process-live-p rustel--process)
    (user-error "This buffer has no active Rustel playback"))
  (process-put rustel--process 'rustel-expected-stop t)
  (interrupt-process rustel--process)
  (message "Sent stop to Rustel (repeat to cut the tail short)"))

(defun rustel--font-lock-keywords ()
  "Return font-lock rules for Rustel lane labels."
  '(("^[ \t]*\\(\\$[[:alnum:]_]*\\)[ \t]*:" 1 font-lock-keyword-face)))

;;;###autoload
(define-derived-mode rustel-mode js-mode "Rustel"
  "Major mode for Rustel/Strudel scores with basic live playback.

\{rustel-mode-map}"
  (rustel--select-utf8-coding)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (font-lock-add-keywords nil (rustel--font-lock-keywords) 'append)
  (add-hook 'after-change-functions #'rustel--after-change nil t)
  (add-hook 'after-save-hook #'rustel--after-save nil t)
  (add-hook 'kill-buffer-hook #'rustel--buffer-killed nil t))

(provide 'rustel-mode)

;;; rustel-mode.el ends here
