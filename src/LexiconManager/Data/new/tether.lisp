;;;;
;;;; W::tether
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::tether
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    )
   )
))

