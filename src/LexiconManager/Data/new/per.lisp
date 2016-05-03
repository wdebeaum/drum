;;;;
;;;; W::PER
;;;;

(define-words :pos W::ADV :templ COUNT-PRED-TEMPL
 :words (
   ((W::PER w::diem)
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-VP-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :wn ("per_diem%1:21:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
;; This group has various non-disc-pre adverbials
  (W::per
   (SENSES
     ((EXAMPLE "gasoline sells for three dollars per gallon")
     (LF-PARENT ONT::iteration-period)
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil)
     (templ binary-constraint-s-templ)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::per
   (SENSES
    ((LF (W::OVER))
     (non-hierarchy-lf t))
    )
   )
))

