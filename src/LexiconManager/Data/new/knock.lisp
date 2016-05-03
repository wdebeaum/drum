;;;;
;;;; w::knock
;;;;

(define-words :pos W::v 
 :words (
((w::knock (w::down))
 (senses
   ((meta-data :origin calo :entry-date 20041122 :change-date 20090504 :comments caloy2)
    (LF-PARENT ONT::decrease)
    (example "knock the price down [to five dollars]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  )
 )
))

