;;;;
;;;; W::DOCTOR
;;;;

(define-words :pos W::n
 :words (
  (W::DOCTOR
  (senses
   ((LF-PARENT ONT::health-professional)
    (TEMPL COUNT-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::name
 :words (
  (w::doctor
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

