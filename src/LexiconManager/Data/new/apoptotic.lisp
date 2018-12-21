;;;;
;;;; W::apoptotic
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::apoptotic
   (SENSES
    (
     (LF-PARENT ONT::BODY-PART-VAL) ; might not matter what the LF-PARENT is since we take the pertainym?
     (SYNTAX (w::pertainym (:* ONT::BIOLOGICAL-PROCESS W::APOPTOSIS)))
     (TEMPL CENTRAL-ADJ-TEMPL)
     )
    )
   )
))
