(require 'slackmacs-utils)

(defun slackmacs-open-conversation (id)
  (setq slackmacs_opened_conversation_id id)
  (let ((inhibit-read-only t))
    (clear-slack-buffer)
    (let ((messages-list (slackmacs/conversation-get (get-token) (get-cookie) id)))
      (let ((parsed-messages (reverse (slackmacs/conversation-parse messages-list ["text" "user"]))))
        (dolist (message parsed-messages)
          (insert (propertize (format "%s: " (gethash (nth 1 message) slackmacs_users_map)) 'face 'bold ))
          (insert (format "%s\n\n" (nth 0 message)) )
        )
      )
    )
  )
)


(provide 'slackmacs-conversation)
