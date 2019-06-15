;;;;
;;;; W::demolish
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::demolish
   (wordfeats (W::morph (:forms (-vb) :nom demolition)))
   (SENSES
    ;((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("demolish%2:37:00"))
     ;(LF-PARENT ONT::evoke-excitement)
     ;(TEMPL agent-affected-xp-templ)
;     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("destroy-44") :wn ("demolish%2:36:00"))
     (LF-PARENT ONT::destroy)
 ; like waste
     )
    )
   )
))

