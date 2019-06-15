;;;;
;;;; W::muddle
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::muddle
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("muddle%2:31:00"))
     (LF-PARENT ONT::evoke-confusion)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
#|
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("amalgamate-22.2-2"))
     (LF-PARENT ONT::associate)
     (TEMPL agent-affected2-templ (xp (% w::pp (w::ptype w::with)))) ; like associate,associate
     )
|#
    )
   )
))

