;;;;
;;;; W::reject
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::reject
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date 20090508 :comments html-purchasing-corpus)
     (LF-PARENT ONT::reject)
     (example "reject the cookie")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )

    (
     (LF-PARENT ONT::REFUTE)
     (example "The result rejected that the gene activates the protein")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )

    (
     (LF-PARENT ONT::REFUTE)
     (example "The result rejected the hypothesis")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )

    )
   )
))

