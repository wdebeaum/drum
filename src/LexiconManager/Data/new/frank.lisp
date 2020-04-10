;;;;
;;;; W::FRANK
;;;;

(define-words :pos W::n
 :words (
  (W::FRANK
  (senses
	   ((LF-PARENT ONT::MEAT-PRODUCT) ;ONT::MEAT-OTHER)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::FRANK
    (wordfeats (W::MORPH (:FORMS (-LY))))
    (SENSES
     ((LF-PARENT ONT::FRANK-val)
      (EXAMPLE "I gave them my frank opinion")
      )
     )
    )
))

