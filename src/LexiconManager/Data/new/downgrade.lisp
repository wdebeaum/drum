;;;;
;;;; w::downgrade
;;;;

(define-words :pos W::v 
 :words (
(w::downgrade
 (senses
   ((meta-data :origin calo :entry-date 20040408 :change-date 20090504 :comments calo-y1v4)
    (example "downgrade the clock speed [to 2.0 ghz]")
   (LF-PARENT ONT::decrease)
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   ;(TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
   )
  )
 )
))

