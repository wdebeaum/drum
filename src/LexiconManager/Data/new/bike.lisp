;;;;
;;;; W::bike
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::bike
   (SENSES
    ((meta-data :origin asma :entry-date 20111004)
     (LF-PARENT ONT::land-vehicle)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::bike
   (SENSES
    ((LF-PARENT ONT::bike)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     (example "he biked across town")
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    )
   )
))

