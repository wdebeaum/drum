;;;;
;;;; W::pertain
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::pertain
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (EXAMPLE "this pertains to that")
     (LF-PARENT ONT::RELATION)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL neutral-neutral-templ (xp (% W::pp (W::ptype W::to))))
     )
    )
   )
))

