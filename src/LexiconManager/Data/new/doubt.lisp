;;;;
;;;; W::doubt
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::doubt
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090501 :comments nil :vn ("judgement-33") :wn ("doubt%2:31:00"))
     (LF-PARENT ont::doubt)
     (example "he doubted that it would rain")
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL experiencer-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ont::doubt)
     (example "he doubted it")
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL experiencer-neutral-xp-templ)
     )

    )
   )
))

