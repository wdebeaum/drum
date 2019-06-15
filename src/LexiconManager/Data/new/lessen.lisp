;;;;
;;;; w::lessen
;;;;

(define-words :pos W::v 
 :words (
(w::lessen
 (senses
  ((meta-data :origin task-learning :entry-date 20050919 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::decrease)
   (example "lessen the indent")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   ;(TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
   )
  ((meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::decrease)
   (example "the pain will lessen in severity / with time")
   (TEMPL AFFECTED-FORMAL-XP-OPTIONAL-TEMPL  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
   )
  )
 )
))

