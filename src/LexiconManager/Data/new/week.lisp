;;;;
;;;; W::WEEK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::WEEK
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::week-duration)
     (templ substance-unit-templ)
     (example "two weeks of work")
     )
    ((meta-data :origin plow :entry-date 20080117 :change-date nil :comments nil :wn ("month%1:28:00"))
     ;(LF-PARENT ONT::time-interval)
     (LF-PARENT ONT::week)
     (templ time-reln-templ)
     (example "next week")
     )
    )
   )
))

