;;;;
;;;; w::convert
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::convert
  (wordfeats (W::morph (:forms (-vb) :nom W::conversion)))
 (senses
   ((meta-data :origin task-learning :entry-date 20050825 :change-date 20090504 :comments nil)
   ;(LF-PARENT ONT::change-format)
   (LF-PARENT ONT::change)
   (example "convert the email to rtf")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   ;(TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? pt w::into W::to)))))
   )
   ))
))

