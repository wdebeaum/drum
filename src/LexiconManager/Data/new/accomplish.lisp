;;;;
;;;; W::accomplish
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::accomplish
   (SENSES
    ;;;; Accomplish the task
    ((LF-PARENT ONT::COMPLETE)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

