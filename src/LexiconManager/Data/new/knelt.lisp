;;;;
;;;; W::knelt
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::knelt
   (wordfeats (W::morph (:forms NIL)) (W::vform (? vf W::past w::pastpart)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("assuming_position-50"))
     (LF-PARENT ONT::body-movement-self)
     (TEMPL agent-templ) ; like lie
     )
    )
   )
))

