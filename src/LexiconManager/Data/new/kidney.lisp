;;;;
;;;; w::kidney
;;;;

(define-words :pos W::n
 :words (
  ((w::kidney w::stone)
  (senses;;;;; names of diseases/conditions that are count nouns and cannot appear without an article
	   ((meta-data :wn ("kidney_stone%1:17:00"))
           (LF-PARENT ONT::kidney-stone)
	    (TEMPL count-pred-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
;; internal
  (W::KIDNEY
  (senses((LF-PARENT ONT::internal-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((W::KIDNEY W::BEAN)
  (senses
	   ((LF-PARENT ONT::BEANS-PEAS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  (W::KIDNEY
  (senses
	   ((LF-PARENT ONT::MEAT)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

