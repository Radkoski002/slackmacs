(require 'slackmacs-utils)
(require 'slackmacs-conversation)

(defun slackmacs-list-conversations ()
  (interactive)
  (open-slack-buffer)
  (let ((conversation_vector (slackmacs/conversation-list-get (get-token) (get-cookie))))
    (let ((parsed_conversations (slackmacs/conversation-list-parse conversation_vector ["id"])))
      (let ((inhibit-read-only t))
        (clear-slack-buffer)
        (dolist (conversation parsed_conversations)
          (insert-text-button 
              (format "%s\n" (nth 0 conversation)) 
              'id (nth 0 conversation)
              'action (lambda (b) (slackmacs-open-conversation (button-get b 'id)))
          )
        )
      )
    )
  )
)


(provide 'slackmacs-conversation-list)

