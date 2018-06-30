;;;;
;;;; w::reimbursement
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::reimbursement
   (SENSES
    ((LF-PARENT ONT::value-COST)
     (example "I got a reimbursement of $10")
     (TEMPL reln-subcat-of-units-TEMPL)
     
     )
    ((LF-PARENT ONT::value-COST)
     (TEMPL other-reln-templ (xp (% W::pp (W::ptype w::for))))
     (example "I got a reimbursement for gift wrapping")
     )
    )
   )))
