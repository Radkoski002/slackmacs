;; -*- lexical-binding: t -*-
(require 'slackmacs-utils)
(require 'slackmacs-conversation)

(defun create-conversation-button (text id)
  (insert-text-button 
    (format "%s\n" text)
    'id id
    'action (lambda (b) (slackmacs-open-conversation (button-get b 'id)))
  )
)

(defun get-cached-conversation-list ()
  (open-conversations-buffer)
  (let ((inhibit-read-only t))
    (clear-slack-buffer)
    (slackmacs/conversation-list-create-buttons slackmacs_instance 'create-conversation-button)
  )
)

(defun get-conversation-list-callback (data)
  (slackmacs/conversation-list-from-json data slackmacs_instance)
  (dolist (buf (match-buffers "slackmacs"))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (clear-slack-buffer)
        (slackmacs/conversation-list-create-buttons slackmacs_instance 'create-conversation-button)
      )
    )
  )
)

(defun get-users-list-callback (data)
  (slackmacs/users-list-from-json data slackmacs_instance)
  (slackmacs-request "conversation-list" 'get-conversation-list-callback)
)

(defun slackmacs-force-refresh-conversation-list ()
  (interactive)
  (if (boundp 'slackmacs_instance)
    (slackmacs-request "users-list" 'get-users-list-callback)
    (message "Please run M-x slackmacs-start first")
  )
)

(defun slackmacs-list-conversations ()
  (interactive)
  (if (boundp 'slackmacs_instance)
    (get-cached-conversation-list)
    (message "Please run M-x slackmacs-start first")
  )
)


(provide 'slackmacs-conversation-list)

