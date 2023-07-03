(require 'slackmacs-module-rs)

(defun benchmark (func arg times)
  (dotimes (i times)
	(append-to-file
	 (format "%s\n"
	    (car (benchmark-run 1 (funcall func arg))))
	 nil "benchmarks.txt")
  ))

(defun benchmark-small-func (func arg test_times bench_times)
  (dotimes (i bench_times)
	(append-to-file
	 (format "%s\n"
	    (car (benchmark-run 1 (dotimes (j test_times)(funcall func arg)))))
	 nil "benchmarks.txt")
  ))

(slackmacs-simulate-error)

(slackmacs-simulate-long-operation)

(benchmark-small-func 'slackmacs-change-case "test" 10 10)

(benchmark 'slackmacs-benchmark-func "test" 10)

