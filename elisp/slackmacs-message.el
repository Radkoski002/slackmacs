(require 'slackmacs-conversation)

(defun slackmacs-message-send ()
  (interactive)

  (if (boundp 'slackmacs_opened_conversation_id)
    (let ((text (read-string "> ")))
      (message (format "Sending message %s to %s" text slackmacs_opened_conversation_id))
      (slackmacs/message-send-text (get-token) (get-cookie) text slackmacs_opened_conversation_id)
      (slackmacs-open-conversation slackmacs_opened_conversation_id)
    )
    (message "No conversation opened")
  )
)

(provide 'slackmacs-message)
