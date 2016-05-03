;;;;
;;;; W::possess
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::possess
   (wordfeats (W::morph (:forms (-vb) :nom w::possession)))
   (SENSES
    ((EXAMPLE "he possesses a truck")
     (LF-PARENT ONT::possess)
     (TEMPL neutral-neutral-templ)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     )
    )
   )
))

