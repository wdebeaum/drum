;;;;
;;;; W::according
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::according W::to)
   (SENSES
    ((LF-PARENT ONT::ATTRIBUTED-INFORMATION)
     (LF-FORM W::ACCORDING-TO)
     (TEMPL binary-constraint-S-templ)
     (EXAMPLE "according to what i have the helicopter takes a half hour")
     )
    )
   )
))

