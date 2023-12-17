(require 'slackmacs-utils)
(require 'slackmacs-request)

(defun create-message-button (sender text ts reply_count)
  (insert-text-button 
    (format "%s%s"
      (propertize (format "%s: " sender) 'face 'bold )
      (propertize (format "%s" text) 'face 'default)
    )
    'text text
    'ts ts
  )
  (insert "\n\t")
  (if (eq reply_count 0)
    ""
    (progn 
      (insert-text-button 
          (propertize 
            (format "Replies: %s" reply_count)
            'face 'link
          )
        'ts ts
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

(defun create-reply-button (sender text ts)
  (insert-text-button 
    (format "%s%s"
      (propertize (format "%s: " sender) 'face 'bold )
      (propertize (format "%s" text) 'face 'default)
    )
    'text text
    'ts ts
  )
  (insert "\n\n")
)

(defun create-back-button ()
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
)

(defun get-conversation-replies-callback (data)
  (let (
    (inhibit-read-only t)
    (conversation_id (slackmacs/conversation-get-id-from-buffer-name (buffer-name)))
    (parent_ts (slackmacs/conversation-get-ts-from-buffer-name (buffer-name)))
    )
      (slackmacs/messages-list-replies-from-json data conversation_id parent_ts slackmacs_instance)
      (clear-current-buffer)
      (create-back-button)
      (slackmacs/messages-list-create-reply-buttons slackmacs_instance conversation_id parent_ts 'create-reply-button)
  )
)


(defun get-conversation-history-callback (data) 
  (let (
    (inhibit-read-only t) 
    (conversation_id (slackmacs/conversation-get-id-from-buffer-name (buffer-name)))
    )
      (slackmacs/messages-list-from-json data conversation_id slackmacs_instance)
      (clear-current-buffer)
      (slackmacs/messages-list-create-message-buttons slackmacs_instance conversation_id 'create-message-button) 
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

(defun slackmacs-refresh-conversation (id)
  (let ((inhibit-read-only t) )
    (clear-current-buffer)
    (slackmacs/messages-list-create-message-buttons slackmacs_instance id 'create-message-button) 
  )
)

(defun slackmacs-refresh-replies (id ts)
  (let ((inhibit-read-only t) )
    (clear-current-buffer)
    (create-back-button)
    (slackmacs/messages-list-create-reply-buttons slackmacs_instance id ts 'create-reply-button)
  )
)

(provide 'slackmacs-conversation)
