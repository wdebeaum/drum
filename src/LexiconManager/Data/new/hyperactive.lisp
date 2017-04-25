;;;;
;;;; w::hyperactive
;;;; 

;(define-words :pos W::adj :templ ADJ-EXPERIENCER-TEMPL
; :words (
;  (W::hyperactive
;   (SENSES
;    ((meta-data :origin medadvisor :entry-date 20060824 :change-date 20080520 :comments nil :wn ("hyperactive%5:00:00:active:00"))
;     (lf-parent ont::physical-symptom-val)
;     (TEMPL central-adj-templ)
;     )
;    )
;   )
;))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::hyperactive
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil)
     (lf-parent ont::hyperactive-val)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))

;(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
; :words (
;  (W::hyperactive  ;; same as "hyper"
;   (wordfeats (W::morph (:FORMS (-LY))))
;   (SENSES
;    ((LF-PARENT ONT::behavioral-property)
;     (SEM (F::GRADABILITY F::+))
;     )
;    )
;   )
;))
