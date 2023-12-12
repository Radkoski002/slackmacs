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
           :user slackmacs-team-domain)
  )

(defun get-cookie ()
  (auth-source-pick-first-password
           :host "slack-cookie"
           :user slackmacs-team-domain)
  )

(defun get-from-cursor ()
  (interactive)
  (message "%s" (button-get (button-at (point)) 'id))
)

(provide 'slackmacs-utils)
