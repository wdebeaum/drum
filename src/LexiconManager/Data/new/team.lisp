;;;;
;;;; W::TEAM
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::TEAM
   (SENSES
    ((meta-data :origin coordops :entry-date 20070511 :change-date nil :comments nil :wn ("team%1:14:00"))
     (LF-PARENT ont::social-group)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::team
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("amalgamate-22.2-2"))
     (LF-PARENT ONT::associate)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% w::pp (w::ptype w::with)))) ; like associate
     )
    )
   )
))

