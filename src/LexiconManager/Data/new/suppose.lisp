;;;;
;;;; W::suppose
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::suppose
   (SENSES
    ((LF-PARENT ONT::SUPPOSE)
     (meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("consider-29.9-2") :wn ("suppose%2:32:00"))
     (example "I suppose that going along the coast is faster")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL experiencer-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::SUPPOSE)
     (SEM (F::Aspect F::stage-level))
     (TEMPL experiencer-neutral-xp-templ)
     )

    )
   )
))

