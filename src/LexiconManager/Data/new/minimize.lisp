;;;;
;;;; w::minimize
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(w::minimize
  (wordfeats (W::morph (:forms (-vb) :nom w::minimization)))
 (senses
  ((meta-data :origin task-learning :entry-date 20050825 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::minimize)
   (example "minimize the window")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   ;(TEMPL AGENT-FORMAL-XP-TEMPL)
   (TEMPL agent-affected-xp-np-templ)
   )
  )
 )
))

