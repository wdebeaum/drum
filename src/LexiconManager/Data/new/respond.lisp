;;;;
;;;; W::respond
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::respond
   (SENSES
    ((LF-PARENT ONT::answer)
     (example "he responded to her [about it]")
     (TEMPL AGENT-to-ADDRESSEE-associated-info-OPTIONAL-TEMPL )
     (meta-data :origin task-learning :entry-date 20050919 :change-date 20090508 :comments nil)
     )
    ((LF-PARENT ONT::answer)
     (example "he responded about it [to her]")
     (TEMPL AGENT-associated-info-ADDRESSEE-OPTIONAL-TEMPL)
     (meta-data :origin task-learning :entry-date 20060405 :change-date 20090508 :comments completeness)
     )
    ((LF-PARENT ONT::answer)
     (example "he responded to the email")
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% w::pp (w::ptype w::to))))
     (meta-data :origin task-learning :entry-date 20050919 :change-date 20090508 :comments nil)
     )
    )
   )
))

