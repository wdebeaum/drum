;;;;
;;;; W::T
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::T w::V)
   (SENSES
    ((LF-PARENT ONT::device)
     (templ bare-pred-templ)
     (SEM (F::mobility F::non-self-moving))
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::T W::BONE W::STEAK)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  (W::T
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

