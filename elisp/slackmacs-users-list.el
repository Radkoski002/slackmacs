(require 'slackmacs-utils)

(defun slackmacs-list-users ()
  (interactive)
  (open-slack-buffer)
  (let ((user_vector (slackmacs/users-list-get (get-token) (get-cookie))))
    (let ((parsed_users (slackmacs/users-list-parse user_vector ["id" "name" "real_name"])))
      (let ((inhibit-read-only t))
        (let ((index 0))
          (dolist (user parsed_users)
            (insert-text-button 
                (format "%s\n" (nth 2 user)) 
                'id (nth 0 user)
                'action (lambda (b) (message (format "%s" (button-get b 'id))))
            )
          )
        )
      )
    )
  )
)


(provide 'slackmacs-users-list)

