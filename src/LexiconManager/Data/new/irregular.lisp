;;;;
;;;; w::irregular
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::IRREGULAR
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date 20090731 :comments nil)
     (LF-PARENT ONT::IRREGULAR)
     (example "he exercises at irregular intervals")
     )
    )
   )
))

#|
(define-words :pos W::n
 :words (
  ((w::irregular w::heartbeat)
  (senses
   ((meta-data :wn ("arrhythmia%1:26:00"))
    (LF-PARENT ONT::medical-symptom)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))
|#
