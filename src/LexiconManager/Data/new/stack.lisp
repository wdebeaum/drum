;;;;
;;;; W::stack
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::stack
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-2"))
     ;(LF-PARENT ONT::fill-container)
     (LF-PARENT ONT::ARRANGING)
     (TEMPL agent-goal-affected-templ (xp (% w::pp (w::ptype (? t w::with))))) ; like load
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-2"))
     ;(LF-PARENT ONT::fill-container)
     (LF-PARENT ONT::ARRANGING)
     (TEMPL agent-affected-goal-optional-templ (xp (% w::pp (w::ptype (? t w::into w::in))))) ; like load
     )
    )
   )
))

