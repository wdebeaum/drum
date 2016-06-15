;;;;
;;;; w::abnormality
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::abnormality
   (SENSES
    ((meta-data :origin chf :entry-date 20070810 :change-date nil :comments nil :wn ("abnormality%1:04:00"))
     (LF-PARENT ONT::abnormality)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::abnormality
  (senses;;;;; names of diseases/conditions that are count nouns and cannot appear without an article
	   ((meta-data :wn ("abnormality%1:26:00"))
           (LF-PARENT ONT::disease)
	    (TEMPL count-pred-TEMPL)
	    )
	   )
)
))

