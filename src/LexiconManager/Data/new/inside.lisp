;;;;
;;;; W::INSIDE
;;;;

(define-words :pos W::ADV
 :words (
  (W::INSIDE
   (SENSES
    ((LF-PARENT ONT::IN-LOC)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (SYNTAX (W::ALLOW-DELETED-COMP +))
     (example "the dog is inside the house")
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
;;;    ))
  ((W::INSIDE W::OF)
   (SENSES
    ((LF-PARENT ONT::IN-LOC)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

