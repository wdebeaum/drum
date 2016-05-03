;;;;
;;;; w::antibiotic
;;;;

(define-words :pos W::n
 :words (
  (w::antibiotic
  (senses
   ((LF-PARENT ont::medication) ;; medication type?
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::n
 :words (
  (w::antibiotic
  (senses
   ((LF-PARENT ont::antibiotic)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

