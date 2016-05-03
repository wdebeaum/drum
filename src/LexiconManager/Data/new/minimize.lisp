;;;;
;;;; w::minimize
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(w::minimize
  (wordfeats (W::morph (:forms (-vb) :nom w::minimum)))
 (senses
  ((meta-data :origin task-learning :entry-date 20050825 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::minimize)
   (example "minimize the window")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-THEME-XP-TEMPL)
   )
  )
 )
))

