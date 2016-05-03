;;;;
;;;; W::receiver
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::receiver
   (SENSES
    ((LF-PARENT ONT::communication-party)
     (templ other-reln-templ)
     (EXAMPLE "The sender and receiver do not share secret information")
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :wn ("receiver%1:18:00") :comments nil)
     )
    )
   )
))

