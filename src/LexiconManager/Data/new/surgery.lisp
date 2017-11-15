;;;;
;;;; W::surgery
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::surgery
   (SENSES
    #|
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("surgery%1:04:01"))
     ;; i2b2 classification -- abstract-object; note that it can also be diagnostic
     (LF-PARENT ont::treatment)
     )
    |#
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("surgery%1:04:01"))
     (LF-PARENT ont::medical-event)
     (example "the doctor performed the surgery")
     )
    )
   )
))

