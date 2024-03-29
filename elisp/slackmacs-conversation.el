(require 'slackmacs-utils)
(require 'slackmacs-request)

(defun create-message-button (sender text ts reply_count)
  (insert-text-button 
    (format "%s%s"
      (propertize (format "%s: " sender) 'face 'bold)
      (propertize (format "%s" text) 'face 'default)
    ) 
    'text text 'ts ts
  )
  (insert "\n")
  (if (eq reply_count 0)
    ""
    (progn 
      (indent-to 4)
      (insert-text-button 
        (propertize (format "Replies: %s" reply_count) 'face 'link)
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
      (propertize (format "%s: " sender) 'face 'bold)
      (propertize (format "%s" text) 'face 'default)
    )
    'text text 'ts ts
  )
  (insert "\n\n")
)

(defun create-conversation-back-button ()
  (insert-text-button 
    (propertize "Back" 'face 'link)
    'action 
    (lambda (_) (slackmacs-list-conversations))
  )
  (insert "\n\n")
)

(defun create-reply-back-button ()
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

(defun slackmacs-update-conversation (id)
  (let ((inhibit-read-only t) (pos (point)))
    (clear-current-buffer)
    (create-conversation-back-button)
    (slackmacs/messages-list-create-message-buttons slackmacs_instance id 'create-message-button) 
    (goto-char pos)
  )

)

(defun slackmacs-update-replies (id ts)
  (let ((inhibit-read-only t) (pos (point)))
    (clear-current-buffer)
    (create-reply-back-button)
    (slackmacs/messages-list-create-reply-buttons slackmacs_instance id ts 'create-reply-button)
    (goto-char pos)
  )
)


(defun get-conversation-replies-callback (data)
  (let (
    (conversation_id (slackmacs/conversation-get-id-from-buffer-name (buffer-name)))
    (parent_ts (slackmacs/conversation-get-ts-from-buffer-name (buffer-name)))
    )
      (slackmacs/messages-list-replies-from-json data conversation_id parent_ts slackmacs_instance)
      (slackmacs-update-replies conversation_id parent_ts)
  )
)

(defun get-conversation-history-callback (data) 
  (let ((conversation_id (slackmacs/conversation-get-id-from-buffer-name (buffer-name))))
    (slackmacs/messages-list-from-json data conversation_id slackmacs_instance)
    (slackmacs-update-conversation conversation_id)
  )
)

(defun slackmacs-force-refresh-conversation (id)
  (slackmacs-request 
    "conversation-history" 
    'get-conversation-history-callback
    `(("channel" . ,id))
  )
)

(defun slackmacs-force-refresh-replies (id ts)
  (slackmacs-request 
    "conversation-replies" 
    'get-conversation-replies-callback
    `(("channel" . ,id) ("ts" . ,ts))
  )
)

(defun slackmacs-open-conversation (id)
  (open-conversation-buffer id)
  (slackmacs-force-refresh-conversation id)
)

(defun slackmacs-open-replies (id ts)
  (open-reply-buffer id ts)
  (slackmacs-force-refresh-replies id ts)
)

(provide 'slackmacs-conversation)
