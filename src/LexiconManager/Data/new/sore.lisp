;;;;
;;;; w::sore
;;;;

(define-words :pos W::n
 :words (
  (w::sore
  (senses((meta-data :wn ("sore%1:26:00"))
          (LF-PARENT ONT::wound)
	    (TEMPL count-pred-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ ADJ-EXPERIENCER-TEMPL
 :words (
  (W::sore
   (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("sore%5:00:00:painful:00"))
     (LF-PARENT ont::physical-symptom-val)
     (templ central-adj-templ)
     )
    )
   )
))

