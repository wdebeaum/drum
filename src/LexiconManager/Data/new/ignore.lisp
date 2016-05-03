;;;;
;;;; W::ignore
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::ignore
     (SENSES
     ((LF-PARENT ONT::FORGET)
      (meta-data :origin plow :entry-date 20050401 :change-date nil :comments nil)
      (example "ignore that option")
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL agent-theme-xp-templ)
      )
     )
     )
))

