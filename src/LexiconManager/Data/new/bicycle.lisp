;;;;
;;;; W::bicycle
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::bicycle
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090422)
     (LF-PARENT ONT::land-vehicle)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::bicycle
   (SENSES
    ((LF-PARENT ONT::bike)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     (example "he bicycled across town")
     )
    )
   )
))
