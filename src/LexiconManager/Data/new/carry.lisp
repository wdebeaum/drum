;;;;
;;;; W::carry
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::carry
   (SENSES
    ((LF-PARENT ONT::TRANSPORT)
     (example "carry the cargo to avon")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
   
   )
)))

