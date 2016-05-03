;;;;
;;;; W::nearly
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::nearly
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::ALMOST)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::ALMOST)
     (example "nearly every truck") ;; but not nearly some/no/few truck !
     (TEMPL QUAN-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::qualification)
     (LF-FORM W::ALMOST)
     (TEMPL PRED-VP-TEMPL)
     )
    ((LF-PARENT ONT::degree-modifier-low)
     (example "you're nearly there" "it's nearly ready")
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcarts-11-1)
     )
    ((LF-PARENT ONT::time-clock-rel)
     (example "it's nearly midnight")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    )
   )
))

