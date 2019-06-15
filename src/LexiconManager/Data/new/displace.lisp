;;;;
;;;; W::displace
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
	  (w::displace
	   (wordfeats (W::morph (:forms (-vb) :nom w::displacement)))
	   (senses
	    ((LF-PARENT ONT::cause-move)
	     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
	     )
	    ))	  
))
