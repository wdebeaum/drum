;;;;
;;;; W::wait
;;;;

(define-words :pos W::v 
 :tags (:base500)
 :words (
;   )
  (W::wait
   (SENSES
    ((LF-PARENT ONT::PAUSE)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TIME-DURATION-TEMPL)
     (example "wait five minutes")
     )
    )
   )
))

