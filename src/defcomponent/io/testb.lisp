
(defpackage :testb
  (:use :common-lisp :comm))

(in-package :testb)

(defun main ()
  (let ((*package* (find-package :testb)))
    (io:send 'register :receiver 'IM :name 'B)
    (loop
      (let ((input (io:recv)))
	(format t "B received: ~S~%" input)
	(let ((reply (list 'answer input)))
	  (format t "B sending: ~S~%" reply)
	  (io:send 'reply :receiver 'a :content reply))))))
      
