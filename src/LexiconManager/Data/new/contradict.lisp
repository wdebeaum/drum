;;;;
;;;; W::contradict
;;;;

(define-words :pos W::v 
 :words (
  (W::contradict
   (SENSES
    (
     (LF-PARENT ONT::REFUTE)
     (example "The result contradicted the hypothesis")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
     )

    (
     (LF-PARENT ONT::REFUTE)
     (example "The result contradicted that the gene activates the protein")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
   ))
))

