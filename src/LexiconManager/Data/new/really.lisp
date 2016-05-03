;;;;
;;;; W::REALLY
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::REALLY
   (SENSES
    ((LF-PARENT ONT::DEGREE-OF-BELIEF)
     (LF-FORM W::REALLY)
     (TEMPL PRED-S-VP-TEMPL)
     )
    ((LF-PARENT ONT::degree-MODIFIER-HIGH)
     (LF-FORM W::REALLY)
     (example "really green")
     (TEMPL ADJ-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::DEGREE-MODIFIER-HIGH)
     (LF-FORM W::REALLY)
     (TEMPL PRED-VP-PRE-TEMPL)
     )
    ((LF-PARENT ONT::intensifier)
     (TEMPL NON-DISC-ADV-OPERATOR-TEMPL)
     (example "he ran really quite fast")
     (PREFERENCE 0.98)  ;;;;; prefer discourse interps if possible
     )
    )
   )
))

