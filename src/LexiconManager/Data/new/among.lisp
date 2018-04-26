;;;;
;;;; W::AMONG
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::AMONG
   (SENSES
    ((meta-data :origin calo :entry-date 20040916 :change-date nil :comments caloy2)
     (LF-PARENT ONT::among)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (example "he walked among them")
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::among
   (SENSES
    ((LF (W::among))
     (non-hierarchy-lf t))
    )
   )
))
