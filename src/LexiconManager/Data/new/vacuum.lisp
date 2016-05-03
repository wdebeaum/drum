;;;;
;;;; W::vacuum
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::vacuum
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil :wn ("vacuum%1:06:00"))
     (LF-PARENT ONT::manufactured-object)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::vacuum
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     (LF-PARENT ONT::clean)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "vaccumm the room")
     )
    )
   )
))

