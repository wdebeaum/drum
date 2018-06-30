;;;;
;;;; W::revenue
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::revenue
   (SENSES
    ((LF-PARENT ONT::value-COST)
     (example "We had revenue of $10")
     (TEMPL reln-subcat-of-units-TEMPL)
     
     )
    ((LF-PARENT ONT::value-COST)
     (TEMPL other-reln-templ (xp (% W::pp (W::ptype w::from))))
     (example "there is an extra revenue from bait dales")
     )
    )
 )))
