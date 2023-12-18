(require 'slackmacs-request)

(defun slackmacs-handle-websocket ()
    (slackmacs-request 
        "get-websocket-updates" 
        (lambda (data) 
            (slackmacs/websocket-handle-events data slackmacs_instance)
            (seq-let (name conversation_id parent_ts) (split-string (buffer-name) "-")
                (if (equal name "conversation")
                    (slackmacs-update-conversation conversation_id)
                )
                (if (equal name "reply")
                    (slackmacs-update-replies conversation_id parent_ts)
                )
            )
        )
    )
)

(provide 'slackmacs-websocket)