;;;;
;;;; W::insulate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (W::insulate
   (SENSES
    ((LF-PARENT ONT::adjust-surface)
     (example "Insulate the room.")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
   ))
))

