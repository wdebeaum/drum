;;;;
;;;; W::intersect
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::intersect
   (SENSES
    ;;;; swier -- I-90 and I-490 intersect
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-plural-templ)
     )
    ;;;; street A intersects/connects/meets with street b
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ (xp (% W::pp (W::ptype W::with))))
     )
    )
   )
))

