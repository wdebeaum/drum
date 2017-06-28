;;;;
;;;; W::halfway
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::halfway
   (SENSES
    ((meta-data :origin step :entry-date 20080528 :change-date nil :comments nil)
     (lf-parent ont::middle-val)
     (example "the halfway point")
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   (W::halfway
   (SENSES
    ((LF-PARENT ONT::degree-modifier-med)
     (example "you're halfway there")
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcarts-11-1)
     )
    )
   )
))

