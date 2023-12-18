;; -*- lexical-binding: t -*-
(require 'slackmacs-conversation)
(require 'slackmacs-request)
(require 'slackmacs-utils)

(defun slackmacs-send-reply-in-thread ()
  (interactive)
  (seq-let (name conversation_id) (split-string (buffer-name) "-")
    (slackmacs/conversation-check-buffer-name name)
    (let ((text (read-string "> ")) (parent_ts (button-get (button-at (point)) 'ts)))
      (slackmacs-request 
        "send-message" 
        (lambda (data) 
            (slackmacs/message-reply-add slackmacs_instance data conversation_id parent_ts)
          )
        `(("text". ,text) ("channel" . ,conversation_id) ("thread_ts" . ,parent_ts))
      )
    )
  )
)

(defun slackmacs-message-send ()
  (interactive)
  (seq-let (name conversation_id parent_ts) (split-string (buffer-name) "-")
    (slackmacs/conversation-check-buffer-name name)
    (let ((text (read-string "> ")))
      (slackmacs-request 
        "send-message" 
        (lambda (data) 
          (if parent_ts
            (progn 
              (slackmacs/message-reply-add slackmacs_instance data conversation_id parent_ts)
              (slackmacs-update-replies conversation_id parent_ts)
            )
            (progn
              (slackmacs/message-add slackmacs_instance data conversation_id)
              (slackmacs-update-conversation conversation_id)
            ) 
          )
        )
        (if parent_ts
          `(("text". ,text) ("channel" . ,conversation_id) ("thread_ts" . ,parent_ts))
          `(("text". ,text) ("channel" . ,conversation_id))
        )
      )
    )
  )
)

(defun slackmacs-message-delete ()
  (interactive)
  (seq-let (name conversation_id parent_ts) (split-string (buffer-name) "-")
    (slackmacs/conversation-check-buffer-name name)
    (let ((message_ts (button-get (button-at (point)) 'ts)))
      (slackmacs-request 
        "delete-message" 
        (lambda (_) 
          (if parent_ts
            (progn 
              (slackmacs/message-reply-delete slackmacs_instance conversation_id message_ts parent_ts)
              (slackmacs-update-replies conversation_id parent_ts)
            )
            (progn 
              (slackmacs/message-delete slackmacs_instance conversation_id message_ts)
              (slackmacs-update-conversation conversation_id)
            )
          )
        )
        (if parent_ts
          `(("ts". ,message_ts) ("channel" . ,conversation_id) ("thread_ts" . ,parent_ts))
          `(("ts". ,message_ts) ("channel" . ,conversation_id))
        )
      )
    )
  )
)

(defun slackmacs-message-edit ()
  (interactive)
  (seq-let (name conversation_id parent_ts) (split-string (buffer-name) "-")
    (slackmacs/conversation-check-buffer-name name)
    (let (
        (text (read-string "> " (button-get (button-at (point)) 'text))) 
        (message_ts (button-get (button-at (point)) 'ts))
      )
        (slackmacs-request 
          "edit-message" 
          (lambda (_) 
            (if parent_ts
              (progn 
                (slackmacs/message-reply-edit slackmacs_instance text conversation_id message_ts parent_ts)
                (slackmacs-update-replies conversation_id parent_ts)
              )
              (progn 
                (slackmacs/message-edit slackmacs_instance text conversation_id message_ts)
                (slackmacs-update-conversation conversation_id)
              )
            )
          )
          (if parent_ts
            `(("text". ,text) ("ts". ,message_ts) ("channel" . ,conversation_id) ("thread_ts" . ,parent_ts))
            `(("text". ,text) ("ts". ,message_ts) ("channel" . ,conversation_id))
          )
        )
    )
  )
)

(provide 'slackmacs-message)
