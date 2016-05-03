;;;;
;;;; W::touching
;;;;

(define-words :pos W::ADV
 :words (
  (W::touching
   (SENSES
    ((LF-PARENT ont::near-reln)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
      (SYNTAX (W::ALLOW-DELETED-COMP +))
     )
    )
   )
))

