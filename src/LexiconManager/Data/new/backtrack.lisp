;;;;
;;;; W::backtrack
;;;;

(define-words :pos W::v 
 :words (
  (W::backtrack
   (SENSES
    ;;;; Backtrack to Avon
    (;(LF-PARENT ONT::GO-BACK)
     (LF-PARENT ONT::RETURN)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-source-optional-TEMPL)
     )
    )
   )
))

