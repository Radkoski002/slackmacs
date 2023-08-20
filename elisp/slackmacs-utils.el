(defun open-slack-buffer ()
  (let ((buffer (generate-new-buffer "slackmacs")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

(defun write-to-buffer (text)
  (with-current-buffer "slackmacs" 
    (goto-char (point-max))
    (insert text)))

(defun write-vector (vec)
  (setq ii 0)
  (while (< ii (length vec))
    (write-to-buffer (format "%s\n" (aref vec ii)))
    (setq ii (1+ ii)))
  )


(defun get-token ()
  (auth-source-pick-first-password
           :host "slack-token"
           :user "test-channel")
  )

(defun get-cookie ()
  (auth-source-pick-first-password
           :host "slack-cookie"
           :user "test-channel")
  )

(provide 'slackmacs-utils)