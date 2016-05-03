
(defpackage "TESTA"
  (:use "COMMON-LISP" "COMM"))

(in-package "TESTA")

(defun main ()
  (let ((*package* (find-package "TESTA")))
    (comm:send 'register :receiver 'IM :name 'A)
    (loop
      (format t "A sleeping for 3 seconds...~%")
      (sleep 3)
      (let ((msg 'hello-from-a))
	(format t "A sending: ~S~%" msg)
	(comm:send 'tell :receiver 'b :content msg))
      (let ((reply (comm:recv)))
	(format t "A received: ~S~%" reply)))))
      
