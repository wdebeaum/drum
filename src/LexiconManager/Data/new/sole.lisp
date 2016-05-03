;;;;
;;;; W::SOLE
;;;;

(define-words :pos W::n
 :words (
  (W::SOLE
  (senses
	   ((LF-PARENT ONT::SALTWATER-fish)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::sole
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("sole%5:00:00:unshared:00" "sole%5:00:00:single:05") :comments html-purchasing-corpus)
     (EXAMPLE "The sole exception")
     (LF-PARENT ONT::cardinality-VAL)
     (SEM (F::GRADABILITY F::-))
     )
    )
   )
))

