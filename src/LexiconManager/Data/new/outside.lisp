;;;;
;;;; W::OUTSIDE
;;;;

(define-words :pos W::ADV
 :words (
  (W::OUTSIDE
   (SENSES
    ((LF-PARENT ONT::OUTSIDE)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (SYNTAX (W::ALLOW-DELETED-COMP +))
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::OUTSIDE W::OF)
   (SENSES
    ((LF-PARENT ONT::OUTSIDE)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

