(require 'slackmacs-request)

(defun slackmacs-start-websocket ()
    (interactive)
    (slackmacs-request "start" (lambda (data) (message data)))
)


(defun slackmacs-get-websocket-updates ()
    (interactive)
    (slackmacs-request "get-websocket-updates" (lambda (data) (slackmacs/websocket-handle-events data)))
)

(provide 'slackmacs-websocket)