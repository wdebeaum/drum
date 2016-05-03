;;;;
;;;; W::relinquish
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::relinquish
   (SENSES
    ((lf-parent ONT::surrender)
     (templ agent-affected-goal-optional-templ)
     (example "I am relinquishing my bedroom to the long-term house guest")
     (meta-data :origin "wordnet-3.0" :entry-date 20090501 :change-date nil :comments nil)
     )
    )
   )
))

