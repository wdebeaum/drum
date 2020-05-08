;;;;
;;;; w::whiskey
;;;;

(define-words :pos W::n
 :words (
  (w::whiskey
  (senses
	   ((LF-PARENT ONT::ALCOHOL-COCKTAILS) ;ALCOHOL)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::WHISKEY W::SOUR)
  (senses
	   ((LF-PARENT ONT::ALCOHOL-COCKTAILS) ;ALCOHOL)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  (w::whiskey
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

