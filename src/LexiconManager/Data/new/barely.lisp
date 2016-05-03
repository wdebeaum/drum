;;;;
;;;; W::barely
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::barely
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::ALMOST)
     (example "barely five trucks")
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::qualification)
      (example "he barely smiled")
     (TEMPL PRED-VP-TEMPL)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    ((LF-PARENT ONT::degree-modifier-low)
     (example "you're barely there")
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    ((LF-PARENT ONT::time-clock-rel)
     (example "it's barely midnight")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    )
   )
))

