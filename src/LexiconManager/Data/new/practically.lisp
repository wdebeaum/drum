;;;;
;;;; W::practically
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   (W::practically
   (SENSES
    ((LF-PARENT ONT::qualification)
     (meta-data :origin cardiac :entry-date 20090120 :change-date nil :comments LM-vocab)
     (TEMPL pred-vp-TEMPL)
     (example "he is practically finished")
     )
     ((LF-PARENT ONT::degree-modifier-med)
     (example "practically never" "practically ready")
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (meta-data :origin cardiac :entry-date 20090120 :change-date nil :comments LM-vocab)
     )
    )
   )
))

