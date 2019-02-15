;;;;
;;;; W::insulate
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
 (W::insulate
   (SENSES
    ((LF-PARENT ONT::adjust-surface)
     (example "Insulate the room.")
     (TEMPL agent-affected-xp-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
   ))
))

