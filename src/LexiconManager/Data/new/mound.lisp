;;;;
;;;; W::mound
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::mound
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-2"))
     (LF-PARENT ONT::fill-container)
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-WITH-2-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? t w::with))))) ; like load
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-2"))
     (LF-PARENT ONT::fill-container)
     (TEMPL agent-affected-goal-optional-templ (xp (% w::pp (w::ptype (? t w::into w::in))))) ; like load
     )
    )
   )
))

