;;;;
;;;; W::UPTO
;;;;

(define-words :pos W::ADV
 :words (
  (W::UPTO
   (SENSES
    ((LF-PARENT ONT::qmodifier)
     (LF-FORM W::up-to)
     (TEMPL number-operator-TEMPL)
     (example "I can spend up to five dollars")
     (meta-data :origin calo :entry-date 20040507 :change-date nil :comments calo-y1variants)
     )

    ((LF-PARENT ONT::UNTIL)
     (TEMPL binary-constraint-s-templ ))
    
     )
   )
))
