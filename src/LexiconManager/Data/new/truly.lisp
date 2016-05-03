;;;;
;;;; W::TRULY
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::TRULY
   (SENSES
    ((LF-PARENT ONT::degree-MODIFIER-VERYHIGH)
     (TEMPL ADJ-OPERATOR-TEMPL)
     (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments y3-test-data)
     )
    ((LF-PARENT ONT::DEGREE-MODIFIER-VERYHIGH)
     (TEMPL PRED-VP-PRE-TEMPL)
     (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments y3-test-data)
     )
    ((LF-PARENT ONT::intensifier)
     (TEMPL NON-DISC-ADV-OPERATOR-TEMPL)
     (example "he ran truly very fast")
     (PREFERENCE 0.98)  ;;;;; prefer discourse interps if possible
     )
    )
   )
))

