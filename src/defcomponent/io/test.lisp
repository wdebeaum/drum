(in-package "USER")

(load "io")
(load "internal")
(load "testa")
(load "testb")

(format t "Starting process A...~%")
(mp:process-run-function "A" #'testa::main)
(format t "Starting process B...~%")
(mp:process-run-function "B" #'testb::main)
