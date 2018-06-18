;;;;
;;;; W::BELOW
;;;;

(define-words :pos W::ADV
 :words (
  (W::BELOW
   (SENSES
    ((LF-PARENT ONT::BELOW)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (SYNTAX (W::ALLOW-DELETED-COMP +))
     )
    ;; modifies a quantity-term but doesn't create its own term
    ((LF-PARENT ONT::QMODIFIER)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     (lf-form w::less)
     (meta-data :origin calo :entry-date 20040426 :change-date nil :comments calo-y1v1)
     )
    ((LF-PARENT ONT::less-than-rel)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (example "purchases below five dollars")
     (SYNTAX (W::ALLOW-DELETED-COMP +) (w::degree-adv +))
     (meta-data :origin calo :entry-date 20040429 :change-date nil :comments calo-y1v1)
     )
    ((LF-PARENT ONT::less-than-rel)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (example "below average temperature")
     )
    ))))

