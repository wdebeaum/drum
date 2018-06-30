;;;;
;;;; W::tax
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::tax
   (SENSES
    ((LF-PARENT ONT::value-COST)
     (example "There is a tax of $10")
     (TEMPL reln-subcat-of-units-TEMPL)
     
     )
    ((LF-PARENT ONT::value-COST)
     (TEMPL other-reln-templ (xp (% W::pp (W::ptype w::on))))
     (example "there is an extra tax on wine")
     )
    )
   )
  ))
