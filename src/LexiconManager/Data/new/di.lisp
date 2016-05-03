;;;;
;;;; W::DI
;;;;

(define-words :pos W::n
 :words (
  ((W::DI W::SODIUM W::PHOSPHATE)
  (senses
	   ((LF-PARENT ONT::PRESERVATIVES)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos w::adv
 :words (
  (w::di-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "diubiquitinate")
    (templ PRED-VP-PRE-templ)
    )
   )
  )
))

(define-words :pos w::adj
 :words (
  (w::di-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "diubiquitination; dipeptide")
    (templ central-adj-templ)
    )
   )
  )
))

(define-words :pos w::adv
 :words (
  (w::di-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "dichromatic")
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))
