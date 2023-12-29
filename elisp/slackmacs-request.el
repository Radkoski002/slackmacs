;; -*- lexical-binding: t -*-
(require 'request)
(require 'slackmacs-utils)

(defun slackmacs-request (path &optional callback req-data)
  (request 
    (format "http:/127.0.0.1:50000/%s" path)
    :headers `(("token" . ,(get-token)) ("cookie" . ,(get-cookie)))
    :data req-data
    :type "POST"
    :success 
    (cl-function
      (lambda (&key data &allow-other-keys)
        (if callback
          (funcall callback data)
        )
      )
    )
  )
)

(provide 'slackmacs-request)
