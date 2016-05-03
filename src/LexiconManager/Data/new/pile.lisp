;;;;
;;;; W::pile
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::pile
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::fill-container)
     (TEMPL agent-affected-goal-optional-templ (xp (% w::pp (w::ptype (? t w::on w::onto w::into w::in))))) ; like pack
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::fill-container)
     (TEMPL agent-goal-affected-templ (xp (% w::pp (w::ptype (? t w::with))))) ; like pack
     )
    
    )
   )
))

