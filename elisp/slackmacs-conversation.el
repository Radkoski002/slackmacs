(require 'slackmacs-utils)
(require 'slackmacs-request)

(defun get-conversation-history-callback (data)
  (let ((inhibit-read-only t))
    (let ((messages-vector (slackmacs/conversation-parse-from-json-string data)))
      (dolist (message messages-vector)
        (let ((message-object (slackmacs/conversation-parse-string-to-message message)))
          (insert (propertize (format "%s: " (gethash (slackmacs/conversation-get-message-sender message-object) slackmacs_users_map)) 'face 'bold ))
          (insert (format "%s\n\n" (slackmacs/conversation-get-message-text message-object)) )
        )
      )
    )
  )
)

(defun slackmacs-open-conversation (id)
  (setq slackmacs_opened_conversation_id id)
  (let ((inhibit-read-only t))
    (clear-slack-buffer)
    (slackmacs-request 
      "conversation-history" 
      'get-conversation-history-callback
      `(("channel" . ,id))
    )
  )
)

(provide 'slackmacs-conversation)
