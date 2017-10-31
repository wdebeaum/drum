;;;;
;;;; W::THUS
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::THUS
   (SENSES
    #|
    ((LF-PARENT ONT::MODIFIER)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     )
    |#
    ((LF-PARENT ONT::therefore)
     (TEMPL pred-s-vp-templ)
     )
    ((LF-PARENT ONT::therefore)
     (TEMPL Binary-constraint-S-subjcontrol-templ)
     )

    )
   )
))
