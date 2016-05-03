;;;;
;;;; W::thaw
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::thaw
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("?thaw%2:30:00"))
     (LF-PARENT ont::atmospheric-event)
     (TEMPL expletive-templ) ; like rain
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("?thaw%2:30:00"))
     (LF-PARENT ont::change-integrity)
     (TEMPL affected-templ)
     (example "the ice thawed")
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("?thaw%2:30:00"))
     (LF-PARENT ont::change-integrity)
     (TEMPL agent-affected-xp-templ)
     (example "I thawed the ice")
     )
    )
   )
))

