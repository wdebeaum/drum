;;;;
;;;; W::glass
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::glass
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("glass%1:06:00"))
    ; (LF-PARENT ONT::tableware)
     (lf-parent ont::glass)
     (TEMPL pred-subcat-contents-templ)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("glass%1:27:00"))
     (LF-PARENT ONT::solid-substance)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

