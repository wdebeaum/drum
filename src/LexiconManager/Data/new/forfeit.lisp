;;;;
;;;; W::forfeit
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::forfeit
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("contribute-13.2-1-1"))
     (LF-PARENT ONT::surrender)
     ;(TEMPL agent-affected-goal-optional-templ) ; like surrender
     (TEMPL AGENT-AFFECTED-AFFECTEDR-XP-OPTIONAL-TEMPL)
     )
    )
   )
))

