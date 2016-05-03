;;;;
;;;; W::PATH
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PATH
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("path%1:06:00"))
     (LF-PARENT ONT::ROUTE)
     )
    ((meta-data :origin ralf :entry-date 20040802 :change-date nil :wn ("path%1:17:00") :comments nil)
     (LF-PARENT ONT::ROUTE)
     (example "the flight path of delta 968")
     (TEMPL OTHER-RELN-of-for-TEMPL)
     )
    )
   )
))

