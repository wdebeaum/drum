;;;;
;;;; W::papa
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::papa
   (SENSES
    ((LF-PARENT ONT::family-relation)
     (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :wn ("father%1:18:00") :comments caloy3-test-data)
     (templ kinship-reln-templ)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (w::papa
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

