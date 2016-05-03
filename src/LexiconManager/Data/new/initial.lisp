;;;;
;;;; W::initial
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::initial
   (SENSES
    ((LF-PARENT ONT::linguistic-object)
     (EXAMPLE "enter the initials for the city")
     (meta-data :origin plow :entry-date 20060706 :change-date nil :wn ("initial%1:10:00") :comments pq)
     )
    )
   )
))

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :words (
  (W::initial
   (SENSES
    ((LF (W::NTH 1))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a))
     )
    )
   )
))

