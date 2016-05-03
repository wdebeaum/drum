;;;;
;;;; w::mrs
;;;;

(define-words :pos W::name
 :words (
  (w::mrs
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

(define-words :pos W::name
 :words (
  ((w::mrs w::punc-period)
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

