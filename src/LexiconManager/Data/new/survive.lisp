;;;;
;;;; W::survive
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::survive
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("exist-47.1-1"))
     (LF-PARENT ONT::survive)
     (TEMPL affected-templ) ; like live
     )
    ((LF-PARENT ONT::enduring)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-neutral-optional-TEMPL)
     (example "he survived the experience")
     )
    )
   )
))

