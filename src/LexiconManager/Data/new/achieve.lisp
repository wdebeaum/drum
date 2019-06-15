;;;;
;;;; W::achieve
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::achieve
   (SENSES
    ;;;; Achieve the goal
    ((LF-PARENT ONT::complete) ;; like attain
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((meta-data :origin step :entry-date 20080625 :change-date nil :comments nil)
     (LF-PARENT ONT::reach)
     (example "the piston achieves its target position")
     (TEMPL agent-neutral-xp-templ)
     )
    )
   )
))

