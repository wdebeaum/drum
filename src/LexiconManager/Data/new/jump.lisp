;;;;
;;;; W::jump
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::jump W::drive)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :comments caloy3)
     (LF-PARENT ont::io-device)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::jump
   (SENSES
    ;;;; jump on the highway, jump the fence
    ((LF-PARENT ONT::jump)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-optional-templ)
     )
    
    )
   )
))

