;;;;
;;;; W::CART
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CART
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((LF-PARENT ONT::vehicle-container)
     
     (meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :comments nil)
     (example "put the banana in the cart")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::cart (W::off))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("drive-11.5-1"))
     (LF-PARENT ONT::transport)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like shuttle
     ;(PREFERENCE 0.96)
     )
    
   )
)))

