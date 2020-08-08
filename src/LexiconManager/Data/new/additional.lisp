;;;;
;;;; W::ADDITIONAL
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::ADDITIONAL
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((LF-PARENT ONT::additional-val)
     (SEM (F::GRADABILITY -))
     (TEMPL central-adj-optional-xp-TEMPL)
     )
    )
   )
))

