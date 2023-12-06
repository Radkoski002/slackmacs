(require 'slackmacs-conversation)
(require 'slackmacs-request)

(defun slackmacs-websocket-test ()
    (interactive) 
    (request "http://127.0.0.1:7878"
      :complete (cl-function
                 (lambda (&key response &allow-other-keys)
                   (message "Done: %s" (request-response-data response)))))
  )

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
