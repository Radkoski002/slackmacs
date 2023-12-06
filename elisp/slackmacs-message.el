(require 'slackmacs-conversation)
(require 'slackmacs-request)

(defun slackmacs-message-send ()
  (interactive)

  (if (boundp 'slackmacs_opened_conversation_id)
    (let ((text (read-string "> ")))
      (slackmacs-request 
        "send-message" 
        (lambda (_) 
          (slackmacs-open-conversation slackmacs_opened_conversation_id)
        )
        `(("text". ,text) ("channel" . ,slackmacs_opened_conversation_id))
      )
    )
    (message "No conversation opened")
  )
)

(provide 'slackmacs-message)
