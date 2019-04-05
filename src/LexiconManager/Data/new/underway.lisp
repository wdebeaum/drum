;;;;
;;;; w::underway
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (w::underway
   (wordfeats (W::ALLOW-POST-N1-SUBCAT +))
   (SENSES
    ((meta-data :origin windenergy :entry-date 20080521 :change-date nil :comments nil)
     (LF-PARENT ONT::occuring-now)
     (SEM (F::GRADABILITY F::+))
     (example "the process is well underway")
     (templ predicative-only-adj-templ)
     )
    )
   )
))

