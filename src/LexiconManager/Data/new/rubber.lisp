;;;;
;;;; W::RUBBER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::RUBBER
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :wn ("rubber%1:27:00" "rubber%1:27:01") :comments html-purchasing-corpus)
     (LF-PARENT ONT::material)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((w::rubber w::duck)
  (senses
   ((LF-PARENT ONT::manufactured-object)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((w::rubber w::ducky)
  (senses
   ((LF-PARENT ONT::manufactured-object)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

