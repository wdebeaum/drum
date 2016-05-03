;;;;
;;;; w::fatten
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::fatten
 (senses
  ((meta-data :origin cardiac :entry-date 20090130 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::swell)
   (example "fatten the budget [to 3K]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::swell)
   (example "it fattened the budget")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL agent-affected-xp-templ)
   )
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::swell)
   (example "it fattened the budget to 32 lbs ")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL agent-affected-result-templ (xp (% W::PP (W::ptype (? pt W::to w::by)))))
   )
  )
 )
))

