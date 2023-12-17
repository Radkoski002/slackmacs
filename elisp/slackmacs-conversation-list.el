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

(defun get-conversation-list-callback (data)
  (open-conversations-buffer)
  (slackmacs/conversation-list-from-json data slackmacs_instance)
  (slackmacs/get-slack-channels slackmacs_instance)
  (let ((inhibit-read-only t))
    (clear-slack-buffer)
    (slackmacs/conversation-list-create-buttons slackmacs_instance 'create-conversation-button)
  )
)

(defun get-users-list-callback (data)
  (if (not (boundp 'slackmacs_instance))
    (setq slackmacs_instance (slackmacs/start))
  )
  (slackmacs/users-list-from-json data slackmacs_instance)
  (slackmacs-request 
    "conversation-list" 
    'get-conversation-list-callback
  )
)

(defun slackmacs-list-conversations ()
  (interactive)
  (slackmacs-request 
    "users-list" 
    'get-users-list-callback
  )
)


(provide 'slackmacs-conversation-list)

