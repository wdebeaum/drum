;;;;
;;;; W::regulate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::regulate
    (wordfeats (W::morph (:forms (-vb) :nom W::regulation :agentnom w::regulator)))
   (SENSES
      (;;(lf-parent ont::managing)
       (lf-parent  ont::manage) ;; 20120521 GUM change new parent 
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "the device regulates the flow of oxygen")
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     )
    ))
))

