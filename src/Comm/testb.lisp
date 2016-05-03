
(defpackage "TESTB"
  (:use "COMMON-LISP" "COMM"))

(in-package "TESTB")

(defun main ()
  (let ((*package* (find-package "TESTB")))
    (comm:send 'register :receiver 'IM :name 'B)
    (loop
      (let ((input (comm:recv)))
	(format t "B received: ~S~%" input)
	(let ((reply (list 'answer input)))
	  (format t "B sending: ~S~%" reply)
	  (comm:send 'reply :receiver 'a :content reply))))))
      
