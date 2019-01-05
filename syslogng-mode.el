;;; syslogng-mode.el --- minor mode for controlling syslog-ng

(defconst syslogng-buffer-name "*syslogng*"
  "default buffer name for syslogng buffer")

(defvar syslogng-buffer nil
  "default buffer for syslogng")

(defvar syslogng-root nil
  "path of the syslogng installation")

(defvar syslogng-config nil
  "syslog-ng configuration file")

(defvar syslogng-mode-modemap-prefix "C-c"
  "prefix of syslogng minor mode keys")

(defun syslogng-autodetect-root ()
  (let ((root-candidates `(,(concat (file-name-directory buffer-file-name) "../")
                           "/" "/usr/" "/opt/")))
    (flet ((probe-root-dir (dir)
                           (when (file-executable-p
                                  (concat
                                   (file-name-as-directory dir) "/sbin/syslog-ng"))
                             (setq syslogng-root dir))))
      (find-if 'probe-root-dir root-candidates))))

(defun syslogng-create-process ()
  (let
      ((binary
        (concat (file-name-as-directory syslogng-root)
                "/sbin/syslog-ng"))
       (conf-file syslogng-config))

    (start-process syslogng-buffer-name syslogng-buffer
                   binary
                   "-Fevd"
                   "-f"
                   conf-file)))

(defun syslogng-create-buffer ()
  "create syslog-ng buffer"
  (when (buffer-live-p syslogng-buffer)
    (kill-buffer syslogng-buffer))

  (setq syslogng-buffer
        (generate-new-buffer syslogng-buffer-name))
  (with-current-buffer syslogng-buffer
    (read-only-mode)
    (local-set-key (kbd "q") 'delete-window))

  syslogng-buffer)

(defun syslogng-start-process ()
  "start syslogng-process in current buffer"
  (lexical-let ((proc (syslogng-create-process)))

    (add-hook 'kill-buffer-hook
              (lambda ()
                (when (process-live-p proc)
                  (kill-process proc))) t t)))

(defun syslogng-start ()
  "start syslogng in a buffer"
  (interactive)

  (syslogng-create-buffer)

  (with-current-buffer syslogng-buffer
    (syslogng-start-process))

  (display-buffer syslogng-buffer))

(defmacro define-ctl-command (cmd)
  `(defun ,(intern (concat "syslogng-" (symbol-name cmd))) ()
     (interactive)
     (let* ((ctl (concat (file-name-as-directory syslogng-root)
                        "/sbin/syslog-ng-ctl"))
            (cmd (format "%s %s" ctl ,(symbol-name cmd))))
       (message cmd)
       (shell-command cmd))))

(define-ctl-command stop)
(define-ctl-command reload)

(defun syslogng-restart ()
  "restart syslogng in a buffer"
  (interactive)

  (syslogng-stop)
  (let ((kill-buffer-query-functions (remove 'process-kill-buffer-query-function kill-buffer-query-functions)))
    (kill-buffer syslogng-buffer))
  (syslogng-start))

(define-minor-mode syslogng-mode
  "control syslogng from buffer"
  :lighter "syslogng"
  :keymap `((,(kbd (concat syslogng-mode-modemap-prefix " s s")) . syslogng-start)
            (,(kbd (concat syslogng-mode-modemap-prefix " s S")) . syslogng-stop)
            (,(kbd (concat syslogng-mode-modemap-prefix " s R")) . syslogng-restart)
            (,(kbd (concat syslogng-mode-modemap-prefix " s r")) . syslogng-reload))

  (unless syslogng-root
    (syslogng-autodetect-root))
  (unless syslogng-config
    (if (string-equal major-mode "syslogngconf-mode")
        (set-variable 'syslogng-config buffer-file-name)
      (set-variable
       'syslogng-config
       (read-file-name "Please select syslog-ng configuration file")))))

(provide 'syslogng-mode)

;;; syslogng-mode.el ends here
