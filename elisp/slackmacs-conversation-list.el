(require 'slackmacs-utils)
(require 'slackmacs-conversation)

(defun get-conversation-list-callback (data)
  (if (not (boundp 'slackmacs_conversation_vector))
    (setq slackmacs_conversation_vector (slackmacs/conversation-list-from-json data))
  )
  (let ((inhibit-read-only t))
    (clear-slack-buffer)
    (dolist (conversation slackmacs_conversation_vector)
      (let ((conversation_object (slackmacs/conversation-from-json conversation)))
        (insert-text-button 
          (format 
            "%s\n" 
            (if 
              (gethash (slackmacs/conversation-get-user-id conversation_object) slackmacs_users_map) 
              (gethash (slackmacs/conversation-get-user-id conversation_object) slackmacs_users_map) 
              (slackmacs/conversation-get-name conversation_object)
            )
          )
          'id (slackmacs/conversation-get-id conversation_object)
          'action (lambda (b) (slackmacs-open-conversation (button-get b 'id)))
        )
      )
    )
  )
)

(defun get-users-list-callback (data)
  (if (not (boundp 'slackmacs_users_vector))
    (setq slackmacs_users_vector (slackmacs/users-list-from-json data))
  )
  (if (not (boundp 'slackmacs_users_map))
    (setq slackmacs_users_map (make-hash-table :test 'equal))
  )
  (dolist (user slackmacs_users_vector)
    (let ((user_object (slackmacs/user-from-json user)))
      (puthash 
        (slackmacs/user-get-id user_object) 
        (slackmacs/user-get-name user_object) 
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

