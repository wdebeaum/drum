;;;;
;;;; w::align
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(w::align
  (wordfeats (W::morph (:forms (-vb) :nom w::alignment)))
 (senses
  ((meta-data :origin task-learning :entry-date 20050815 :change-date 20090507 :comments nil)
   (LF-PARENT ONT::arranging)
   (example "align text within a text box")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-xp-templ)
   )
  )
 )
))

