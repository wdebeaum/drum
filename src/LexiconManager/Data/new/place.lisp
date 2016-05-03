;;;;
;;;; W::PLACE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::PLACE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("place%1:15:00" "place%1:15:04"))
     (LF-PARENT ONT::PLACE)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::PLACE
   (SENSES
    ((EXAMPLE "Place the order")
     (LF-PARENT ONT::submit)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((LF-PARENT ONT::put)
     (meta-data :origin plow :entry-date 20050318 :change-date nil :comments nil)
     (example "place the book on the table")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

