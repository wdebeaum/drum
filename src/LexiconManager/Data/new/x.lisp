;;;;
;;;; w::x
;;;;

(define-words :pos W::n
 :words (
  ((w::x w::ray)
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
  (W::X
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

