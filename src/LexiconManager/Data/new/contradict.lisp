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
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )

    (
     (LF-PARENT ONT::REFUTE)
     (example "The result contradicted that the gene activates the protein")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
   ))
))

