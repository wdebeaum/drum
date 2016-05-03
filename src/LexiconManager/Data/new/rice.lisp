;;;;
;;;; w::rice
;;;;

(define-words :pos W::n :templ MASS-PRED-TEMPL
 :words (
;; food words with no morphology
  (w::rice
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin food-kb :entry-date 20050801 :change-date nil :wn ("rice%1:13:00") :comments nil)
     (LF-PARENT ont::grains)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::rice
  (senses
	   ((LF-PARENT ONT::GRAINS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

