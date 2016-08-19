;;;;
;;;; W::pile
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::pile
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::amass)
     (TEMPL agent-affected-goal-templ)
     )
    )
   )
  ))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  ((W::pile w::up)
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::amass)
     (TEMPL affected-templ)
     )
    )
   )
  ))
