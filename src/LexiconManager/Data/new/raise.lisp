;;;;
;;;; W::raise
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::raise
   (SENSES
    ((LF-PARENT ONT::MOVE-upward)
     (example "raise the banana a little")
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (meta-data :origin fruitcarts :entry-date 20050401 :change-date nil :comments fruitcart-07-2 :vn ("put_direction-9.4") :wn ("raise%2:38:00"))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180706 :change-date nil :comments nil)
     (lf-parent ont::nurturing)
     (example "He was raised by his deaf mother.")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

