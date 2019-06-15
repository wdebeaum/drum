;;;;
;;;; W::irritate
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::irritate
   (wordfeats (W::morph (:forms (-vb) :nom w::irritation)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("irritate%2:37:00"))
     (LF-PARENT ONT::evoke-bother)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ;((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
     ;(LF-PARENT ONT::evoke-discomfort)
     ;(TEMPL agent-affected-xp-templ)
;     )
    )
   )
))