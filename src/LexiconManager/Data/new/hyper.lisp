;;;;
;;;; W::HYPER
;;;;

(define-words :pos w::adv
 :words (
  (w::hyper-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-HIGH)
    (example "hyperactivate")
    (templ PRED-VP-PRE-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::hyper-
   (SENSES
    (
     (LF-PARENT ONT::DEGREE-MODIFIER-HIGH)
     (example "hyperactivation; hypermarket")
     (TEMPL central-adj-templ)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::hyper-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-HIGH)
    (example "hyperclever; hypersensitive")
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::hyper  ;; short for "hyperactive"
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((LF-PARENT ONT::behavioral-property)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))
