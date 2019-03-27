;;;;
;;;; w::degrade
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::degrade
 (wordfeats (W::morph (:forms (-vb) :nom w::degradation)))
 (senses
  ((meta-data :origin cardiac :entry-date 20081223 :change-date 20090504 :comments LM-vocab)
   (LF-PARENT ONT::deteriorate)
   (example "has his condition degraded" "the land has degraded")
   (templ affected-templ)
   )
  ((meta-data :origin cardiac :entry-date 20081223 :change-date 20090504 :comments LM-vocab)
   (LF-PARENT ONT::deteriorate)
   (example "it degraded the result")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL agent-affected-xp-templ)
   )
  ((meta-data :origin cardiac :entry-date 20081223 :change-date 20090504 :comments LM-vocab)
   (LF-PARENT ONT::deteriorate)
   (example "it degraded the pressure to 32 lbs per ")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL agent-affected-result-templ (xp (% W::PP (W::ptype W::to))))
   )
  )
 )
))

