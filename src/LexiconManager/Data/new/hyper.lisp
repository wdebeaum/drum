;;;;
;;;; W::HYPER
;;;;

(define-words :pos w::adv
 :words (
  (w::hyper-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-HIGH)
    (example "hyperactivate")
    (templ V-PREFIX-templ)
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
     (TEMPL prefix-adj-templ)
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
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::hyper  ;; short for "hyperactive"
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((LF-PARENT ONT::hyperactive-val)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))
