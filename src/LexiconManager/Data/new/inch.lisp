;;;;
;;;; W::INCH
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::INCH
   (SENSES
    ((meta-data :origin calo :entry-date 20060803 :change-date nil :comments nil :wn ("inch%1:23:00"))
     (LF-PARENT ONT::length-unit)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :tags (:base500)
 :words (
  (W::inch
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("inch%2:38:00"))
     (LF-PARENT ONT::move-slowly)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

