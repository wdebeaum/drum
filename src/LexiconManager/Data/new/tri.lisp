;;;;
;;;; W::TRI
;;;;

(define-words :pos W::n
 :words (
  ((W::TRI W::TIP)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::TRI W::TIP W::ROAST)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos w::adv
 :words (
  (w::tri-
  (senses
   ((lf-parent ont::tri-val)
    (example "")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos w::adj
 :words (
  (w::tri-
  (senses
   ((lf-parent ont::tri-val)
    (example "triphosphate")
    (templ prefix-adj-templ)
    )
   )
  )
))

(define-words :pos w::adv
 :words (
  (w::tri-
  (senses
   ((lf-parent ont::tri-val)
    (example "")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
