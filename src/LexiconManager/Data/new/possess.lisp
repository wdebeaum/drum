;;;;
;;;; W::possess
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::possess
   (wordfeats (W::morph (:forms (-vb) :nom w::possession)))
   (SENSES
    ((EXAMPLE "he possesses a truck")
     (LF-PARENT ONT::possess)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     ;;(SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     )
    )
   )
))

