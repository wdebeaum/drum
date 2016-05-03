;;;;
;;;; w::dr
;;;;

(define-words :pos W::name
 :words (
  (w::dr
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

(define-words :pos W::name
 :words (
  ((w::dr w::punc-period)
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

