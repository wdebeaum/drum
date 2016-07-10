;;;;
;;;; W::STRAIGHT
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::STRAIGHT
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("straight%5:00:00:continuous:00"))
;     (LF-PARENT ONT::VERTICAL)
     (LF-PARENT ONT::ROUTE-TOPOLOGY-VAL)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
;   )
  (W::STRAIGHT
   (SENSES
    ((LF-PARENT ONT::MODIFIER)
     (TEMPL BINARY-CONSTRAINT-ADV-OPERATOR-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::STRAIGHT
   (SENSES
    ((LF-PARENT ONT::DIRECTION)
     (TEMPL PRED-S-POST-TEMPL)     
     )
    )
   )
))

