;;;;
;;;; W::EMAIL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::EMAIL
   (SENSES
    ((meta-data :origin boudreaux :entry-date 20031024 :wn ("email%1:10:00"))
     (LF-PARENT ONT::email)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::email
   (SENSES
    ((LF-PARENT ONT::SEND)
     (example "email the image to him" "email him the message")
     (templ agent-affected-recipient-alternation-templ)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050919 :change-date nil :comments nil)
     )
    )
   )
))

