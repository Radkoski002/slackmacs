(require 'slackmacs-utils)
(require 'slackmacs-conversation)

(defun slackmacs-list-conversations ()
  (interactive)
  (open-slack-buffer)
  (makunbound 'slackmacs_opened_conversation_id)
  (if (not (boundp 'slackmacs_users_vector))
    (setq slackmacs_users_vector (slackmacs/users-list-get (get-token) (get-cookie)))
  )
  (if (not (boundp 'slackmacs_parsed_users))
    (setq slackmacs_parsed_users (slackmacs/users-list-parse slackmacs_users_vector ["id" "name"] ))
  )
  (if (not (boundp 'slackmacs_users_map))
    (setq slackmacs_users_map (make-hash-table :test 'equal))
  )
  (dolist (parsed_user slackmacs_parsed_users)
    (puthash (nth 0 parsed_user) (nth 1 parsed_user) slackmacs_users_map)
  )
  (if (not (boundp 'slackmacs_conversation_vector))
    (setq slackmacs_conversation_vector (slackmacs/conversation-list-get (get-token) (get-cookie)))
  )
  (if (not (boundp 'slackmacs_parsed_conversations))
    (setq 
      slackmacs_parsed_conversations 
      (slackmacs/conversation-list-parse 
        slackmacs_conversation_vector 
        ["id" "name" "user"]
      )
    )
  )
  (let ((inhibit-read-only t))
    (clear-slack-buffer)
    (dolist (conversation slackmacs_parsed_conversations)
      (insert-text-button 
        (format 
          "%s\n" 
          (if 
            (gethash (nth 2 conversation) slackmacs_users_map) 
            (gethash (nth 2 conversation) slackmacs_users_map) 
            (nth 1 conversation)
          )
        )
        'id (nth 0 conversation)
        'action (lambda (b) (slackmacs-open-conversation (button-get b 'id)))
      )
    )
  )
)


(provide 'slackmacs-conversation-list)

