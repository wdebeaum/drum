;;;;
;;;; W::intersect
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::intersect
   (SENSES
    ;;;; swier -- I-90 and I-490 intersect
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NP-PLURAL-TEMPL)
     )
    ;;;; street A intersects/connects/meets with street b
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::pp (W::ptype W::with))))
     )
    )
   )
))

