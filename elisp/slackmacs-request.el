;; -*- lexical-binding: t -*-
(require 'request)
(require 'slackmacs-utils)

(defun slackmacs-test-request ()
  (interactive)
  (slackmacs-request "test" (lambda (data) (message "%s" data)) '(("foo" . "bar")))
)

(defun slackmacs-request (path &optional callback req-data)
  (if (not req-data)
    (setq req-data '())
  )
  (request 
    (format "http:/127.0.0.1:3000/%s" path)
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
