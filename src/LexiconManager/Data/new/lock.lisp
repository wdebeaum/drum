;;;;
;;;; w::lock
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (w::lock
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :wn ("lock%1:06:00") :comments caloy3)
     (LF-PARENT ONT::device)
     (example "are there security locks on the doors")
     (templ count-pred-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::lock
   (SENSES
    ((LF-PARENT ONT::device)
     (EXAMPLE "a security header with a lock appears")
     (meta-data :origin task-learning :entry-date 20050817 :change-date nil :wn ("lock%1:06:00") :comments nil)
     )
    )
   )
))

