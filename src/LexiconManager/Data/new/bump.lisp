;;;;
;;;; w::bump
;;;;

(define-words :pos W::n
 :words (
  (w::bump
  (senses((LF-PARENT ONT::wound)
	    (TEMPL count-pred-TEMPL)
	    )
	   )
)
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
((w::bump (w::up))
 (senses
   ((meta-data :origin calo :entry-date 20050323 :change-date 20090504 :comments caloy2)
    (LF-PARENT ONT::increase)
    (example "bump the price up [to five dollars]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  )
 )
))

