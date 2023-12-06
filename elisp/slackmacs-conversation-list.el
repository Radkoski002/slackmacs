(require 'slackmacs-utils)
(require 'slackmacs-conversation)

(defun get-conversation-list-callback (data)
  (if (not (boundp 'slackmacs_conversation_vector))
    (setq slackmacs_conversation_vector (slackmacs/conversation-list-parse-from-json-string data))
  )
  (let ((inhibit-read-only t))
    (clear-slack-buffer)
    (dolist (conversation slackmacs_conversation_vector)
      (let ((conversation_object (slackmacs/conversation-list-parse-string-to-conversation conversation)))
        (insert-text-button 
          (format 
            "%s\n" 
            (if 
              (gethash (slackmacs/conversation-list-get-conversation-user conversation_object) slackmacs_users_map) 
              (gethash (slackmacs/conversation-list-get-conversation-user conversation_object) slackmacs_users_map) 
              (slackmacs/conversation-list-get-conversation-name conversation_object)
            )
          )
          'id (slackmacs/conversation-list-get-conversation-id conversation_object)
          'action (lambda (b) (slackmacs-open-conversation (button-get b 'id)))
        )
      )
    )
  )
)

(defun get-users-list-callback (data)
  (if (not (boundp 'slackmacs_users_vector))
    (setq slackmacs_users_vector (slackmacs/users-list-parse-from-json-string data))
  )
  (if (not (boundp 'slackmacs_users_map))
    (setq slackmacs_users_map (make-hash-table :test 'equal))
  )
  (dolist (user slackmacs_users_vector)
    (let ((user_object (slackmacs/users-list-parse-string-to-user user)))
      (puthash 
        (slackmacs/users-list-get-user-id user_object) 
        (slackmacs/users-list-get-user-name user_object) 
        slackmacs_users_map
      )
    )
  )
  (slackmacs-request 
    "conversation-list" 
    'get-conversation-list-callback
  )
)

(defun slackmacs-list-conversations ()
  (interactive)
  (open-slack-buffer)
  (makunbound 'slackmacs_opened_conversation_id)

  (slackmacs-request 
    "users-list" 
    'get-users-list-callback
  )
)


(provide 'slackmacs-conversation-list)

