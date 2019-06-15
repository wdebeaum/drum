;;;;
;;;; W::negotiate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::negotiate
   (SENSES
    ((LF-PARENT ONT::negotiate)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "they negotiated a deal")
     (TEMPL AGENT-result-TEMPL)
     )
    )
   )
))
