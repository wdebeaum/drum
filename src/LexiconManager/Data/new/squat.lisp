;;;;
;;;; W::squat
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::squat
   (wordfeats (W::morph (:forms (-vb) :past W::squatted :ing W::squatting)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("assuming_position-50"))
     (LF-PARENT ONT::body-movement-self)
     (TEMPL agent-templ) ; like lie
     )
    )
   )
))

