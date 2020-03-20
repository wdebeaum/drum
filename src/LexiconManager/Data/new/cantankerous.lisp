;;;;
;;;; W::cantankerous
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::cantankerous
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSEs
    ((example "his landlord is cantankerous" "she has a cantankerous landlord")
     (LF-PARENT ONT::NEGATIVE-DISPOSITION-VAL)
     (templ central-adj-templ)
    )

;    ((example "she has a cantankerous landlord")
;     (LF-PARENT ONT::NEGATIVE-DISPOSITION-VAL)
;     (templ central-adj-templ)
;    )

   ))
))

