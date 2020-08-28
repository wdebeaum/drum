;;;;
;;;; W::TREND
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::trend
   (SENSES
    ((LF-PARENT ONT::direction)
     (TEMPL OTHER-RELN-TEMPL (xp (% w::pp (w::ptype w::in)
				    (W::lf (% ?p (w::class (? x ont::abstract-object ont::situation-root))))
				    )))
     (example "the trend in fashion")
     )
    )
   )
))

