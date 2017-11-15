;;;;
;;;; W::operation
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::operation
   (SENSES
    #|
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("operation%1:04:00"))
     (LF-PARENT ont::treatment)
     )
    |#
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("surgery%1:04:01"))
     (LF-PARENT ont::medical-event)
     (example "the doctor performed the operation")
     )
    )
   )
))

