(defun open-conversations-buffer ()
  (let ((buffer (get-buffer-create "slackmacs")))
    (switch-to-buffer buffer)
    (read-only-mode)
    ))

(defun open-conversation-buffer (id)
  (let ((buffer (get-buffer-create (concat "conversation-" id))))
    (switch-to-buffer buffer)
    (read-only-mode)
    ))

(defun open-reply-buffer (id ts)
  (let ((buffer (get-buffer-create (concat "reply-" id "-" ts))))
    (switch-to-buffer buffer)
    (read-only-mode)
    ))


(defun clear-current-buffer ()
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      )))

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

(defun get-button-at-cursor ()
  (interactive)
  (message "%s" (button-get (button-at (point)) 'id))
)

(provide 'slackmacs-utils)
