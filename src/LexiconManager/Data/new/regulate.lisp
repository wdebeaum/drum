;;;;
;;;; W::regulate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::regulate
    (wordfeats (W::morph (:forms (-vb) :nom W::regulation :agentnom w::regulator)))
   (SENSES
      (;;(lf-parent ont::managing)
        ;(lf-parent  ont::manage) ;; 20120521 GUM change new parent 
       (lf-parent  ont::governing)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "the device regulates the flow of oxygen")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     )
    ))
))

