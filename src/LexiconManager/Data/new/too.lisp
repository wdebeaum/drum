;;;;
;;;; W::TOO
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::TOO
   (SENSES
    ((LF-PARENT ONT::ADDITIVE)
     (LF-FORM W::TOO)
     (TEMPL PRED-S-POST-TEMPL)
     )
    ((LF-PARENT ONT::ADDITIVE)
     (LF-FORM W::TOO)
     (TEMPL PRED-NP-subj-TEMPL)
     (PREFERENCE 0.98)
     )
    ((LF-PARENT ONT::DEGREE-MODIFIER-HIGH)
     (LF-FORM W::TOO)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     )
    )
   )
))

