;;;;
;;;; w::pulmonary
;;;;

(define-words :pos W::n
 :words (
  ((w::pulmonary w::edema)
  (senses
   ((meta-data :wn ("edema%1:26:00"))
    (LF-PARENT ONT::medical-symptom)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::adj
 :words (
  (w::pulmonary
  (senses
   ((LF-PARENT ONT::body-part-val)
    (TEMPL central-adj-templ)
    (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
    )
   )
)
))

