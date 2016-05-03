;;;;
;;;; W::unhook
;;;;

(define-words :pos W::v 
 :words (
  (W::unhook
   (SENSES
    ;;;; swier -- unhook the boxcar
    ((LF-PARENT ONT::UNATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected-SOURCE-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
     )
    )
   )
))

