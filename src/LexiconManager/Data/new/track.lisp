;;;;
;;;; W::track
;;;;

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
  (W::track
   (SENSES
    ((EXAMPLE "Start tracking my location")
     (LF-PARENT ONT::FOLLOW)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

