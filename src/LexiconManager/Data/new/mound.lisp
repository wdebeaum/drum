;;;;
;;;; W::mound
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::mound
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-2"))
     (LF-PARENT ONT::fill-container)
     (TEMPL agent-goal-affected-templ (xp (% w::pp (w::ptype (? t w::with))))) ; like load
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-2"))
     (LF-PARENT ONT::fill-container)
     (TEMPL agent-affected-goal-optional-templ (xp (% w::pp (w::ptype (? t w::into w::in))))) ; like load
     )
    )
   )
))

