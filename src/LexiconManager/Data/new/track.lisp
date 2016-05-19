;;;;
;;;; W::track
;;;;

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
  (W::track
   (SENSES
    ((EXAMPLE "He tracked the deer.  Start tracking my location")
     (LF-PARENT ONT::PURSUE)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

