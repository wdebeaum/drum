;;;;
;;;; W::INLAND
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
;   )
  (W::INLAND
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("inland%3:00:00"))
     (LF-PARENT ONT::GEO-FEATURE-VAL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::INLAND
   (SENSES
    ((LF-PARENT ONT::DIRECTION)
     (TEMPL PRED-S-POST-TEMPL)
     )
    )
   )
))

