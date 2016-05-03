;;;;
;;;; W::contain
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::contain
   (SENSES
    ((LF-PARENT ONT::CONTAINMENT)
      (EXAMPLE "The truck contains the cargo")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-templ)
     )
    )
   )
))

