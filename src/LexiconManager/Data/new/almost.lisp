;;;;
;;;; W::ALMOST
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   (W::ALMOST
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::ALMOST)
     (example "almost five trucks")
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::ALMOST)
     (example "almost every truck")
     (TEMPL QUAN-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::qualification)
     (LF-FORM W::ALMOST)
     (example "he almost smiled" "he is almost there")
     (TEMPL PRED-VP-TEMPL)
     )
    ((LF-PARENT ONT::degree-modifier-low)
     (example "almost never" "almost ready")
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (meta-data :origin cardiac :entry-date 20080730 :change-date nil :comments speech-pretest)
     )
    ((LF-PARENT ONT::time-clock-rel)
     (example "it's almost midnight")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    )
   )
))

