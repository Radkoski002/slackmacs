(require 'slackmacs-conversation)
(require 'slackmacs-request)
(require 'slackmacs-utils)

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

(defun slackmacs-message-delete ()
  (interactive)

  (if (boundp 'slackmacs_opened_conversation_id)
    (let ((message-ts (button-get (button-at (point)) 'ts)))
      (slackmacs-request 
        "delete-message" 
        (lambda (_) 
          (slackmacs-open-conversation slackmacs_opened_conversation_id)
        )
        `(("ts". ,message-ts) ("channel" . ,slackmacs_opened_conversation_id))
      )
    )
    (message "No conversation opened")
  )
)

(defun slackmacs-message-edit ()
  (interactive)
  (if (boundp 'slackmacs_opened_conversation_id)
    (let ((text (read-string "> ")) (message-ts (button-get (button-at (point)) 'ts)))
      (slackmacs-request 
        "edit-message" 
        (lambda (_) 
          (slackmacs-open-conversation slackmacs_opened_conversation_id)
        )
        `(("text". ,text) ("ts". ,message-ts) ("channel" . ,slackmacs_opened_conversation_id))
      )
    )
    (message "No conversation opened")
  )
)

(provide 'slackmacs-message)
