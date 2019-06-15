;;;;
;;;; W::respond
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::respond
   (SENSES
    ((LF-PARENT ONT::answer)
     (example "he responded to her [about it]")
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP2-OPTIONAL-TEMPL )
     (meta-data :origin task-learning :entry-date 20050919 :change-date 20090508 :comments nil)
     )
    ((LF-PARENT ONT::answer)
     (example "he responded about it [to her]")
     (TEMPL AGENT-FORMAL-AGENT1-2-XP1-PP-3-XP2-PP-TO-OPTIONAL-TEMPL)
     (meta-data :origin task-learning :entry-date 20060405 :change-date 20090508 :comments completeness)
     )
    ((LF-PARENT ONT::answer)
     (example "he responded to the email")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::pp (w::ptype w::to))))
     (meta-data :origin task-learning :entry-date 20050919 :change-date 20090508 :comments nil)
     )
    )
   )
))

