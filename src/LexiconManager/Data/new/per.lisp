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

(define-words :pos W::ADV :templ COUNT-PRED-TEMPL
 :words (
   ((W::PER w::cent)
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-VP-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :wn ("per_diem%1:21:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::n 
 :words (
   ((W::per w::cent)
    ;;(wordfeats (W::morph (:forms (-s-3p) :plur W::percent)))
   (SENSES
    ((LF-PARENT ONT::percent)
     (meta-data :origin calo :entry-date 20041206 :change-date nil :wn ("percent%1:24:00") :comments caloy2)
     (example "this cpu is 20 per cent faster")
     (TEMPL SUBSTANCE-UNIT-TEMPL)
     ;;(templ other-reln-theme-templ)
     )
    )
   )
))
