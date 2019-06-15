;;;;
;;;; W::figure
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::figure
   (SENSES
    ((LF-PARENT ONT::shape-object)
     (EXAMPLE "she is watching her figure")
     (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::figure (W::out))
   (SENSES
    ;;;; I figured out that...
    ((LF-PARENT ONT::determine)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     (meta-data :origin bee :entry-date 20040609 :change-date nil :comments portability-expt)
     )
    ((LF-PARENT ONT::solve)
     (example "figure out a plan" "figure out when to go")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

