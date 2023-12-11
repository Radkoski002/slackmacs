(require 'slackmacs-request)

(defun slackmacs-start-websocket ()
    (interactive)
    (slackmacs-request "start" (lambda (data) (message data)))
)


(defun slackmacs-get-websocket-updates ()
    (interactive)
    (slackmacs-request "get-websocket-updates" (lambda (data) (message data)))
)

(provide 'slackmacs-websocket)