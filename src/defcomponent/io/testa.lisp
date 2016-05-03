
(defpackage :testa
  (:use :common-lisp :comm))

(in-package :testa)

(defun main ()
  (let ((*package* (find-package :testa)))
    (io:send 'register :receiver 'IM :name 'A)
    (loop
      (format t "A sleeping for 3 seconds...~%")
      (sleep 3)
      (let ((msg 'hello-from-a))
	(format t "A sending: ~S~%" msg)
	(io:send 'tell :receiver 'b :content msg))
      (let ((reply (io:recv)))
	(format t "A received: ~S~%" reply)))))
      
