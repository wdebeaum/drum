;;;;
;;;; W::CLOSEBY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::CLOSEBY
   (SENSES
    ((LF-PARENT ONT::near)
     (example "the houses are close by")
     (TEMPL ADJ-THEME-TEMPL)
     (SEM (f::orientation F::neg) (f::intensity ont::hi))
     )
    )
   )
))

