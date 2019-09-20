;;;;
;;;; W::leak
;;;;

(define-words :pos W::v 
 :words (
  (W::leak
   (SENSES
    ((EXAMPLE "The water leaked from the can.")
     (LF-PARENT ONT::come-out-of)
     (TEMPL AFFECTED-TEMPL)     
     )
    ((EXAMPLE "The can leaked the water.")
     (LF-PARENT ONT::emit-giveoff-discharge)
     (TEMPL agent-affected-xp-templ)
     )    
    )
)))

