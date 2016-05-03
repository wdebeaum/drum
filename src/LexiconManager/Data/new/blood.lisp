;;;;
;;;; W::blood
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::blood
   (SENSES
    ((meta-data :origin medadvisor :entry-date 20060803 :change-date 20070827 :comments nil :wn ("blood%1:08:00"))
     (LF-PARENT ONT::bodily-fluid)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
;; internal
  ((w::blood w::cell)
  (senses((LF-PARENT ONT::internal-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

