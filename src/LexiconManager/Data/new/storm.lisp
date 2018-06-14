;;;;
;;;; W::STORM
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::STORM
   (SENSES
    ((meta-data :origin plow :entry-date 20060803 :change-date nil :comments nil :wn ("storm%1:19:00"))
     (LF-PARENT ont::storm)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::storm
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("storm%2:43:00" "storm%2:43:01"))
     (LF-PARENT ont::atmospheric-event)
     (TEMPL expletive-templ) ; like rain
     )
    )
   )
))

