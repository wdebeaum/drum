;;;;
;;;; W::ABOVE
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::ABOVE
   (SENSES
    ((LF-PARENT ONT::ABOVE)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (SYNTAX (W::ALLOW-DELETED-COMP +))
     )
    ((LF-PARENT ONT::QMODIFIER)
     (TEMPL NUMBER-OPERATOR-TEMPL)
      (lf-form w::more)
      (meta-data :origin calo :entry-date 20040426 :change-date nil :comments calo-y1v1)
      )
    ((LF-PARENT ONT::more-than-rel)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (example "purchases above five dollars")
     (SYNTAX (W::ALLOW-DELETED-COMP +) (w::degree-adv +))
     (meta-data :origin calo :entry-date 20040112 :change-date nil :comments calo-y1v1)
     )
    ((LF-PARENT ONT::more-than-rel)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (example "above average temperature")
     )
    )
   )
))

