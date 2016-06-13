;;;;
;;;; W::donate
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::donate-give
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("contribute-13.2-1-1"))
     (LF-PARENT ONT::donate)
     (TEMPL agent-affected-goal-optional-templ) ; like surrender
     )
    )
   )
))

