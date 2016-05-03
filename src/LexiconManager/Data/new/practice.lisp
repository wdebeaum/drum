;;;;
;;;; w::practice
;;;;

(define-words :pos W::n
 :words (
  (w::practice
  (senses
   ((LF-PARENT ONT::practice)
    (meta-data :origin cardiac :entry-date 20090129 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::practice
   (SENSES
    ((LF-PARENT ONT::practicing)
     (example "practice on this page")
     (SEM (F::Cause F::agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::PP (W::ptype W::on))))
     )
    ((LF-PARENT ONT::practicing)
     (example "practice (that)")
     (SEM (F::Cause F::agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-optional-TEMPL)
     )
    )
   )
))

