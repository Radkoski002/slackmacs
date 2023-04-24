(require 'emacs-slack)

(defun rust-test ()
  (interactive)
  (em-slack-say-hello
   (read-from-minibuffer "Who should I greet?: ")
  )
)

(defun rust-test-2 ()
  (interactive)
  (em-slack-say-hello
    (thing-at-point 'line)
  )
)


(defun replace-line ()
  (interactive)
  (let ((content (thing-at-point 'line)))
    (delete-region
	(line-beginning-position)
	(line-end-position)
    )
    (insert (em-slack-change-case content))
  )
)

