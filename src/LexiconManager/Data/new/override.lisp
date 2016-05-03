;;;;
;;;; W::override
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::override
   (SENSES
    ((LF-PARENT ONT::replacement)
     (templ other-reln-theme-templ)
     (EXAMPLE "remove the style overrides")
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :wn ("override%1:06:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::override
   (SENSES
    ((LF-PARENT ONT::Replacement)
     (example "override a page's font")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :comments nil)
     )
    )
   )
))

