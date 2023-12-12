(require 'slackmacs-utils)
(require 'slackmacs-request)

(defun get-conversation-replies-callback (data)
  (let ((inhibit-read-only t))
    (clear-current-buffer)
    (insert-text-button 
      (propertize "Back" 'face 'link)
      'action 
      (lambda (_) 
        (slackmacs-open-conversation 
          (slackmacs/conversation-get-id-from-buffer-name (buffer-name))
        )
      )
    )
    (insert "\n\n")
    (let ((messages-vector (slackmacs/conversation-get-messages data)))
      (dolist (message messages-vector)
        (let ((message-object (slackmacs/message-from-json message)))
          (insert-text-button 
            (format "%s%s"
              (propertize (format "%s: " (gethash (slackmacs/message-get-sender message-object) slackmacs_users_map)) 'face 'bold )
              (propertize (format "%s" (slackmacs/message-get-text message-object)) 'face 'default)
            )
            'text (slackmacs/message-get-text message-object)
            'ts (slackmacs/message-get-ts message-object)
          )
          (insert "\n\n")
        )
      )
    )
  )
)


(defun get-conversation-history-callback (data) 
  (let ((inhibit-read-only t))
      (clear-current-buffer)
      (let ((messages-vector (slackmacs/conversation-get-messages-reversed data)))
        (dolist (message messages-vector)
          (let ((message-object (slackmacs/message-from-json message)))
            (insert-text-button 
              (format "%s%s"
                (propertize (format "%s: " (gethash (slackmacs/message-get-sender message-object) slackmacs_users_map)) 'face 'bold )
                (propertize (format "%s" (slackmacs/message-get-text message-object)) 'face 'default)
              )
              'text (slackmacs/message-get-text message-object)
              'ts (slackmacs/message-get-ts message-object)
            )
            (insert "\n\t")
            (if (eq (slackmacs/message-get-reply-count message-object) 0)
              ""
              (progn 
                (insert-text-button 
                    (propertize 
                      (format "Replies: %s" (slackmacs/message-get-reply-count message-object))
                      'face 'link
                    )
                  'ts (slackmacs/message-get-ts message-object)
                  'action 
                  (lambda (b) 
                    (slackmacs-open-replies 
                      (slackmacs/conversation-get-id-from-buffer-name (buffer-name)) 
                      (button-get b 'ts)
                    )
                  )
                )
                (insert "\n")
              )
            )
            (insert "\n")
          )
        )
      )
    )
)

(defun slackmacs-open-conversation (id)
  (open-conversation-buffer id)
  (slackmacs-request 
    "conversation-history" 
    'get-conversation-history-callback
    `(("channel" . ,id))
  )
)


(defun slackmacs-open-replies (id ts)
  (open-reply-buffer id ts)
  (slackmacs-request 
    "conversation-replies" 
    'get-conversation-replies-callback
    `(("channel" . ,id) ("ts" . ,ts))
  )
)

(provide 'slackmacs-conversation)
