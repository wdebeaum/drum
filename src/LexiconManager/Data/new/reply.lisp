;;;;
;;;; W::reply
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::reply
   (SENSES
    ((LF-PARENT ONT::letter-mail)
     (EXAMPLE "compose a reply")
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :wn ("reply%1:10:01") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::reply
   (SENSES
    ((LF-PARENT ONT::answer)
     (example "he replied to her [about it]")
     ;;(TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::to))))
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP2-OPTIONAL-TEMPL )
     (meta-data :origin task-learning :entry-date 20050818 :change-date 20090506 :comments nil)
     )
    ((LF-PARENT ONT::answer)
     (example "he replied to the email")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::pp (w::ptype w::to))))
     (meta-data :origin task-learning :entry-date 20050818 :change-date 20090506 :comments nil)
     )
    )
   )
))

