;; -*- lexical-binding: t -*-
(require 'slackmacs-conversation)
(require 'slackmacs-request)
(require 'slackmacs-utils)

(defun slackmacs-message-send ()
  (interactive)

  (seq-let (name conversation_id reply_ts) (split-string (buffer-name) "-")
    (slackmacs/conversation-check-buffer-name name)
    (let ((text (read-string "> ")))
      (slackmacs-request 
        "send-message" 
        (lambda (_) 
          (if reply_ts
            (slackmacs-open-replies conversation_id reply_ts)
            (slackmacs-open-conversation conversation_id)
          )
        )
        (if reply_ts
          `(("text". ,text) ("channel" . ,conversation_id) ("thread_ts" . ,reply_ts))
          `(("text". ,text) ("channel" . ,conversation_id))
        )
      )
    )
  )
)

(defun slackmacs-message-delete ()
  (interactive)
  (seq-let (name conversation_id reply_ts) (split-string (buffer-name) "-")
    (slackmacs/conversation-check-buffer-name name)
    (let ((message-ts (button-get (button-at (point)) 'ts)))
      (slackmacs-request 
        "delete-message" 
        (lambda (_) 
          (if reply_ts
            (slackmacs-open-replies conversation_id reply_ts)
            (slackmacs-open-conversation conversation_id)
          )
        )
        (if reply_ts
          `(("ts". ,message-ts) ("channel" . ,conversation_id) ("thread_ts" . ,reply_ts))
          `(("ts". ,message-ts) ("channel" . ,conversation_id))
        )
      )
    )
  )
)

(defun slackmacs-message-edit ()
  (interactive)
  (seq-let (name conversation_id reply_ts) (split-string (buffer-name) "-")
    (slackmacs/conversation-check-buffer-name name)
    (let (
        (text (read-string "> " (button-get (button-at (point)) 'text))) 
        (message-ts (button-get (button-at (point)) 'ts))
      )
        (slackmacs-request 
          "edit-message" 
          (lambda (_) 
            (if reply_ts
              (slackmacs-open-replies conversation_id reply_ts)
              (slackmacs-open-conversation conversation_id)
            )
          )
          (if reply_ts
            `(("text". ,text) ("ts". ,message-ts) ("channel" . ,conversation_id) ("thread_ts" . ,reply_ts))
            `(("text". ,text) ("ts". ,message-ts) ("channel" . ,conversation_id))
          )
        )
    )
  )
)

(provide 'slackmacs-message)
