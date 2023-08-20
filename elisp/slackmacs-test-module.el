(defun rust-test ()
  (interactive)
  (slackmacs-say-hello
   (read-from-minibuffer "Who should I greet?: ")
  )
)

(defun rust-test-2 ()
  (interactive)
  (slackmacs-test-say-hello-2
   (read-from-minibuffer "Who should I greet?: ")
  )
)

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

(defun slackmacs-list-users ()
  (interactive)
  (open-slack-buffer)
  (write-vector (slackmacs-get-users-list (get-token) (get-cookie)))
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

(provide 'slackmacs-test-module)

