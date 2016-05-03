;;;;
;;;; W::TRIP
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::TRIP
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("trip%1:04:00"))
     (LF-PARENT ONT::TRIP)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::trip
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090121 :change-date nil :comments nil)
     (LF-PARENT ONT::fall)
     (TEMPL affected-TEMPL)
     (example "he tripped over his shoes")
     )
    )
   )
))

