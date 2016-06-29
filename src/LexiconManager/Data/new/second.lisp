;;;;
;;;; W::SECOND
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::SECOND
;   (abbrev w::s)
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("second%1:28:00"))
     (LF-PARENT ONT::second-duration)
     (templ unit-templ)
     )
    ((meta-data :origin plow :entry-date 20080117 :change-date nil :comments nil :wn ("month%1:28:00"))
     (LF-PARENT ONT::time-interval)
     (templ time-reln-templ)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::SECOND
   (SENSES
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     (TEMPL PRED-S-TEMPL)
     (PREFERENCE 0.96)
     )
    )
   )
))

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :tags (:base500)
 :words (
  (W::SECOND
   (SENSES
    ((LF (W::NTH 2))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a)(w::ntype w::day))
     )
    )
   )
))

