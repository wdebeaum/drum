(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::succeed
   (SENSES
    ((LF-PARENT ONT::succeed)
      (example "he succeeded")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-TEMPL))
    ((LF-PARENT ONT::succeed)
      (example "he succeeded in singing")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-EFFECT-SUBJCONTROL-TEMPL  (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype (? pt w::at w::in))))))
    ))
   ))
