(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
	 (W::succeed
	  (wordfeats (W::morph (:forms (-vb) :nom w::success)))
	  (SENSES
	   (;(LF-PARENT ONT::succeed)
	    (LF-PARENT ONT::COMPLETE)
	    (example "he succeeded")
	    (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
	    (TEMPL AGENT-TEMPL))
	   (;(LF-PARENT ONT::succeed)
	    (LF-PARENT ONT::COMPLETE)
	    (example "he succeeded in singing")
	    (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
	    (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL  (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype (? pt w::at w::in))))))
	   ))
	 ))
