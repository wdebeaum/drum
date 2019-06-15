;;;;
;;;; W::proceed
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::proceed
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("begin-55.1"))
     (LF-PARENT ONT::activity-ongoing)
     (TEMPL AGENT-NEUTRAL-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::with)))) ; like go-on
     
     )
    
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("begin-55.1"))
     (LF-PARENT ONT::activity-ongoing)
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL) ; like go-on
     
     )
    ((LF-PARENT ONT::GO-ON)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     (example "proceed to the exit")
     (preference 0.98)
     )
    )
   )
))

