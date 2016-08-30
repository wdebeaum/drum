;;;;
;;;; W::NEVER
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::NEVER
   (SENSES
    (;(LF-PARENT ONT::FREQUENCY)
     (LF-PARENT ONT::never)
     (TEMPL PRED-S-POST-TEMPL)
     (SYNTAX (W::NEG +))
     )
    (;(LF-PARENT ONT::FREQUENCY)
     (LF-PARENT ONT::never)
     (TEMPL PRED-S-VP-TEMPL)
     (SYNTAX (W::NEG +))
     )
    (;(LF-PARENT ONT::FREQUENCY)
     (LF-PARENT ONT::never)
     (TEMPL PRED-VP-PRE-TEMPL)
     (SYNTAX (W::NEG +))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::never W::mind)
   (SENSES
    ((LF (W::NEVER-MIND))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_REJECT))
     )
    )
   )
))

