;;;;
;;;; W::quantity
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::quantity
   (SENSES
    ((meta-data :origin calo :entry-date 20031229 :change-date nil :wn ("quantity%1:03:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::measure-scale)
     (example "a quantity of five pounds")
;     (SEM (F::scale F::scale))
     (SEM (F::scale (? sc ont::scale ont::domain)))
     (TEMPL reln-subcat-of-units-TEMPL)
     (preference .96) ;;prefer canonical sense
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("quantity%1:07:00"))
     ;(LF-PARENT ONT::quantity)
     (LF-PARENT ONT::quantity-abstr)
     (TEMPL OTHER-RELN-TEMPL)
     (example "a quantity of water/people")
     )
    )
   )
  ))

