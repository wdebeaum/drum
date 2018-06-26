;;;;
;;;; W::attribute
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::attribute
   (SENSES
    ((LF-PARENT ont::attribute)
     (TEMPL OTHER-RELN-TEMPL)
     (EXAMPLE "They can specify these and other attributes individually")
     (meta-data :origin task-learning :entry-date 20050817 :change-date nil :wn ("attribute%1:09:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::attribute
   (SENSES
    ((LF-PARENT ont::attribute-impute)
     (TEMPL agent-neutral-to-neutral-templ)
     (EXAMPLE "They attribute the crop failure to drought")
     )
    )
   )
))
