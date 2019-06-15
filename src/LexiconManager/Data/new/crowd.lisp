;;;;
;;;; W::CROWD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CROWD
   (SENSES
     ((LF-PARENT ONT::social-group)
     (TEMPL classifier-count-pl-templ)
     (example "the crowd of people cheered")
     (PREFERENCE 0.97)
     )
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s7)
     (LF-PARENT ONT::social-group)
     (example "the crowd cheered")
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::crowd
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::fill-container)
     (TEMPL agent-affected-goal-optional-templ (xp (% w::pp (w::ptype (? t w::on w::into w::in))))) ; like pack
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::fill-container)
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-WITH-2-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? t w::with))))) ; like pack
     )
    
   )
)))

