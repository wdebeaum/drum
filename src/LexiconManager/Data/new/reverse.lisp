;;;;
;;;; W::reverse
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::reverse
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :wn ("reverse%1:24:00") :comments caloy3)
     (lf-parent ont::relation)
     (example "we thought he was happy but the reverse was true")
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::reverse
   (SENSES
    ((LF-PARENT ONT::ROTATE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :comments nil)
     (example "reverse the sort order")
     )
    )
   )
))

