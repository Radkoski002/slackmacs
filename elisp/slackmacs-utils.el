(defun open-slack-buffer ()
  (let ((buffer (get-buffer-create "slackmacs")))
    (switch-to-buffer buffer)
    (read-only-mode)
    ))

(defun clear-slack-buffer ()
  (let ((buffer (get-buffer-create "slackmacs")))
    (with-current-buffer buffer
      (erase-buffer)
      )))

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
