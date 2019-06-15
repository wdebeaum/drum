;;;;
;;;; W::attach
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::attach
     (wordfeats (W::morph (:forms (-vb) :nom w::attachment)))
   (SENSES
    ;;;; attach the paper to the wall
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    )
   )
))
