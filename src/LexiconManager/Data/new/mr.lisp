;;;;
;;;; w::mr
;;;;

(define-words :pos W::name
 :words (
  (w::mr
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

(define-words :pos W::name
 :words (
  ((w::mr W::punc-period)
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

