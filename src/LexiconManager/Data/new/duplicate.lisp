;;;;
;;;; W::duplicate
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::duplicate
   (SENSES
    ((meta-data :origin task-learning :entry-date 20051028 :change-date nil :wn ("duplicate%1:06:00") :comments calo-y1variants)
     (example "make a duplicate")
     (LF-PARENT ONT::phys-representation)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
   (W::duplicate
    (SENSES
     ((meta-data :origin task-learning :entry-date 20050817 :change-date nil :comments nil)
      (EXAMPLE "duplicate the text")
      (LF-PARENT ONT::duplicate)
      (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      )
     )
    )
))

