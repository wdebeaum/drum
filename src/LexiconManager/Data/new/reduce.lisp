;;;;
;;;; w::reduce
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::reduce
   (wordfeats (W::morph (:forms (-vb) :nom w::reduction)))
 (senses
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::decrease)
   (example "reduce the clock speed [to 1ghz]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  ((meta-data :origin step :entry-date 20081031 :change-date 20090504 :comments step5)
   (LF-PARENT ONT::decrease)
   (example "this storage reduced the stability")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL agent-affected-xp-templ)
   )
  ((meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::decrease)
   (example "it reduced in speed")
   (templ affected-theme-xp-optional-templ  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
   )
  )
 )
))

