;;;;
;;;; W::PAYMENT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PAYMENT
   (SENSES
    ((LF-PARENT ONT::value-COST)
     (example "I sent a payment of $10")
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    ((LF-PARENT ONT::value-COST)
     (TEMPL other-reln-templ (xp (% W::pp (W::ptype w::for))))
     (example "Here is the payment for gift wrapping")
     )
    ))))

