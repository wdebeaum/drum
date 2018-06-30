;;;;
;;;; W::rebate
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::rebate
   (SENSES
    ((LF-PARENT ONT::value-COST)
     (example "I got a rebate of $10")
     (TEMPL reln-subcat-of-units-TEMPL)
         )
    ((LF-PARENT ONT::value-COST)
     (TEMPL other-reln-templ (xp (% W::pp (W::ptype w::on))))
     (example "there is an extra rebate on gift wrapping")
     )
    )
   )
))

