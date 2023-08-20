(require 'slackmacs-utils)

(defun slackmacs-list-users ()
  (interactive)
  (open-slack-buffer)
  (write-vector (slackmacs-get-users-list (get-token) (get-cookie)))
  )

(provide 'slackmacs-test-module)

