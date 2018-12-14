;;;;
;;;; W::nap
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::nap
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("nap%1:28:00"))
     (LF-PARENT ONT::sleep)
     (example "take a nap")
     )
    )
   )
))

(define-words :pos W::V 
 :words (
   (W::nap
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("snooze-40.4") :wn ("nap%2:29:00"))
     (LF-PARENT ONT::sleep)
     (TEMPL affected-templ) ; like sleep
     )
    )
   )
))

