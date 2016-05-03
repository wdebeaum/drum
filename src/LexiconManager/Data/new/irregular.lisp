;;;;
;;;; w::irregular
;;;;

(define-words :pos W::n
 :words (
  ((w::irregular w::heartbeat)
  (senses
   ((LF-PARENT ONT::physical-symptom)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::IRREGULAR
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date 20090731 :comments nil)
     (LF-PARENT ONT::OCCASIONAL)
     (example "he exercises at irregular intervals")
     )
    )
   )
))

