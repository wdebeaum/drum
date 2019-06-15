;;;;
;;;; w::diminish
;;;;

(define-words :pos W::v 
 :words (
  (w::diminish
   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date 20090504 :comments nil :vn ("calibratable_cos-45.6-1" "other_cos-45.4") :wn ("diminish%2:30:00"))
     (LF-PARENT ONT::decrease)
     (example "diminish the clock speed [to 1Ghz]")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

