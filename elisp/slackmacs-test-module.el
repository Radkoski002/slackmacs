(defun rust-test ()
  (interactive)
  (slackmacs-say-hello
   (read-from-minibuffer "Who should I greet?: ")
  )
)

(defun rust-test-2 ()
  (interactive)
  (slackmacs-say-hello
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
    (insert (slackmacs-change-case content))
  )
)

(defun thread-test ()
  (interactive)
  (slackmacs-thread-test)
  )

(defun signal-test ()
  (interactive)
  (slackmacs-simulate-long-operation)
  )

(defun buffer-test ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (generate-new-buffer "test-buffer")
  (switch-to-buffer "test-buffer")
  )

(defun write-to-file (text)
  (append-to-file (format "%s\n" text) nil "~/Coding/Studia/INZ/slackmacs/benchmark.txt")
  )

(defun slackmacs-benchmark-func-elisp ()
  (upcase (buffer-string))
  )

(defun benchmark-slackmacs(func-name bench-repetitions iterations)
  (write-to-file
    (format "Benchmarking %s with %s repetitions %s times" func-name bench-repetitions iterations) 
  )
  (write-to-file
    (format "buffer_size:  %s" (buffer-size))
  )
  (dotimes (i iterations)
    (write-to-file
      (benchmark-run bench-repetitions (funcall func-name)) 
    )
  )
  )

(defun benchmark-func-mixed ()
  (interactive)
  (benchmark-slackmacs 
    'slackmacs-benchmark-func 
    (string-to-number (read-from-minibuffer "Number of benchmark repetitions: "))
    100)
  )

(defun benchmark-func-rust ()
  (interactive)
  (benchmark-slackmacs 
    'slackmacs-benchmark-func-rust
    (string-to-number (read-from-minibuffer "Number of benchmark repetitions: "))
    100)
  )

(defun benchmark-func-elisp ()
  (interactive)
  (benchmark-slackmacs 
    'slackmacs-benchmark-func-elisp
    (string-to-number (read-from-minibuffer "Number of benchmark repetitions: "))
    100)
  )

(provide 'slackmacs-test-module)

