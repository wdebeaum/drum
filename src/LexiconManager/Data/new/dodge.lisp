;;;;
;;;; W::dodge
;;;;

(define-words :pos W::V :templ agent-NEUTRAL-xp-templ
 :words (
  (W::dodge
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("avoid-52") :wn ("dodge%2:32:00" "dodge%2:38:00"))
     (LF-PARENT ONT::avoiding)
 ; like circumvent,evade,avoid
     )
   
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("avoid-52") :wn ("dodge%2:32:00" "dodge%2:38:00"))
     (LF-PARENT ONT::avoiding)
     (TEMPL agent-action-subjcontrol-templ (xp (% w::vp (w::vform w::ing)))) ; like avoid
     )
    )
   )
))

