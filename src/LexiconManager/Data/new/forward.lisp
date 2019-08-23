;;;;
;;;; W::forward
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::forward
   (SENSES
    ((LF-PARENT ONT::SEND)
     (example "forward the email to him" "forward him the email")
     (TEMPL AGENT-AFFECTED-TEMPL)
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
     (TEMPL PRED-S-OR-NP-POST-TEMPL)
     ;(TEMPL PRED-S-POST-TEMPL)
     ;(TEMPL PRED-S-VP-templ)
     ;;(templ PREDICATIVE-ONLY-ADJ-TEMPL)
     (example "Move it forward.")
     )
    )
   )
))

