;;;;
;;;; W::YOUNG
;;;;

(define-words :pos W::n
 :words (
  ((W::YOUNG W::TOM)
  (senses
	   ((LF-PARENT ONT::TURKEY)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ LESS-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::YOUNG
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    (
     (lf-parent ont::young-val)
     (SEM (F::GRADABILITY F::+) (f::orientation ont::less) (f::intensity ont::med))
     )
    )
   )
))

