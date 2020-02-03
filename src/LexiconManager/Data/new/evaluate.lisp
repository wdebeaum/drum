;;;;
;;;; W::evaluate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::evaluate
   (wordfeats (W::morph (:forms (-vb) :nom W::evaluation)))
   (SENSES
    ((LF-PARENT ONT::evaluate)
     (TEMPL agent-neutral-xp-templ)
     (EXAMPLE "evaluate the problem")
     (meta-data :origin task-learning :entry-date 20050830 :change-date nil :comments nil)
     )
    )
   )
))

