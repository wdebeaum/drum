;;;;
;;;; W::CLOSEBY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::CLOSEBY
   (SENSES
    ((LF-PARENT ONT::DISTANCE-VAL)
     (example "the houses are close by")
     (TEMPL ADJ-THEME-TEMPL)
     (SEM (f::orientation ont::less) (f::intensity ont::hi))
     )
    )
   )
))

