;;;;
;;;; W::abort
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::abort
   (SENSES
    ((LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected-XP-TEMPL)
     (example "abort the mission")
     )
    ((LF-PARENT ONT::DESTROY)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected-XP-TEMPL)
     (example "abort the fetus")
     )
    #|((LF-PARENT ONT::DIE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-TEMPL)
     (example "abort")
     (meta-data :origin plot :entry-date 20080604 :change-date nil :comments nil)
     )|#
    )
   )
))

