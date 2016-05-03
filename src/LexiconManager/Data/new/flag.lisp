;;;;
;;;; W::flag
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::flag
   (SENSES
    ((LF-PARENT ONT::flag)
     (meta-data :origin fruitcarts :entry-date 20041103 :change-date nil :comments nil)
     (example "under the flag")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (w::flag
   (SENSES
    ((EXAMPLE "flag that message")
     (LF-PARENT ONT::highlight)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     )
    )
   )
))

