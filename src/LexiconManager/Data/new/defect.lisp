;;;;
;;;; w::defect
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::defect
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::problem)
     )
))))


#|
(define-words :pos W::n
 :words (
  (w::defect
  (senses;;;;; names of diseases/conditions that are count nouns and cannot appear without an article
	   ((meta-data :wn ("defect%1:26:01"))
           (LF-PARENT ONT::medical-disorders-and-conditions)
	    (TEMPL count-pred-TEMPL)
	    )
	   )
)
))
|#

