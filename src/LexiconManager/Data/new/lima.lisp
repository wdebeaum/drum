;;;;
;;;; W::LIMA
;;;;

(define-words :pos W::n
 :words (
  ((W::LIMA W::BEAN)
  (senses
	   ((LF-PARENT ONT::BEANS-PEAS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  (w::lima
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

