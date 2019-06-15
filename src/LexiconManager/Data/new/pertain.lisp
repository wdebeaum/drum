;;;;
;;;; W::pertain
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::pertain
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (EXAMPLE "this pertains to that")
     (LF-PARENT ONT::RELATE)
     ;(SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    )
   )
))

