;;;;
;;;; W::recap
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::recap
   (SENSES
    ;;;; recap the information
    ((LF-PARENT ONT::REPEAT)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    ;;;; He recapped
    ((LF-PARENT ONT::repeat)
     (TEMPL agent-templ)
     )
    ;;;; he recapped on the injured (monroe s2)
    ((LF-PARENT ONT::REPEAT)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-PP-ABOUT-FOR-TEMPL (xp (% W::pp (W::ptype W::on))))
     )
    )
   )
))

