;;;;
;;;; W::ENOUGH
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::ENOUGH
   (SENSES
    ;; should this be the quantifier? *the enough trucks
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("enough%5:00:00:sufficient:00"))
     (EXAMPLE "That's enough [for him]")
     (LF-PARENT ONT::ADEQUATE)
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::for))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("enough%5:00:00:sufficient:00"))
     (example "there are trucks enough")
     (LF-PARENT ONT::ADEQUATE)
     (SEM (F::GRADABILITY F::-))
     (TEMPL postpositive-adj-optional-xp-templ)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  ;; Moved words from below
  (W::ENOUGH
   (SENSES
    ((LF-PARENT ONT::degree-modifier-med)
     (TEMPL PRED-S-POST-TEMPL)
     (example "I've talked about it enough")
     (PREFERENCE 0.97)  ;; prefer postpositive interp if available
     )
   ((LF-PARENT ONT::enough-val)
     (templ postpositive-adv-optional-xp-templ)
     (meta-data :origin calo :entry-date 20050216 :change-date nil :comments caloy2)
     (example "is it quiet enough")
     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::enough
   (wordfeats (W::negatable +))
   (SENSES
    ((LF ONT::enough)
     (example "enough of the trucks")
     (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-templ) 
     (SYNTAX (W::agr (? agr w::3s W::3p)) (w::status w::indefinite-plural))
     )
    ((LF ONT::enough)
     (example "enough of the water")
     (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
     (SYNTAX (W::agr (? agr W::3s)) (w::status w::indefinite)) ; -- never plural if mass
     )
    )
   )
))

