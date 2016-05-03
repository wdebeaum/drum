;;;;
;;;; w::xray
;;;;

(define-words :pos W::n
 :words (
  (w::xray
  (senses
   ((LF-PARENT ONT::medical-diagnostic)
    (meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil)
    (TEMPL COUNT-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  (w::xray
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

