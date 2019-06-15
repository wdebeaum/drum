;;;;
;;;; W::unhitch
;;;;

(define-words :pos W::v 
 :words (
  (W::unhitch
   (SENSES
    ;;;; swier -- unhich the boxcar
    ((LF-PARENT ONT::UNATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
     )
    )
   )
))

