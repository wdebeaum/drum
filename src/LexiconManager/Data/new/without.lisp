;;;;
;;;; W::WITHOUT
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::WITHOUT
   (SENSES
    ((meta-data :origin calo :entry-date 20040414 :change-date nil :comments calo-y1v4)
     (example "not without downgrading something else")
     (LF-PARENT ONT::without)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::without
   (SENSES
    ((LF (W::WITHOUT))
     (non-hierarchy-lf t))
    )
   )
))

