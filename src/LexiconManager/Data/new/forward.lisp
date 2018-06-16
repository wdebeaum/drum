;;;;
;;;; W::forward
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::forward
   (SENSES
    ((LF-PARENT ONT::SEND)
     (example "forward the email to him" "forward him the email")
     (templ agent-affected-recipient-alternation-templ)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :comments nil :vn ("send-11.1-1"))
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::FORWARD
   (SENSES
    ((LF-PARENT ONT::DIRECTION-forward)
     (TEMPL PRED-S-POST-TEMPL)
     )
    )
   )
))

