;;;;
;;;; W::numb
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::numb
   (SENSES
    ;((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("numb%2:39:00"))
     ;(LF-PARENT ONT::evoke-calm)
     ;(EXAMPLE "")
     ;(TEMPL agent-affected-xp-templ)
;     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
     (LF-PARENT ONT::evoke-numbness)
     (EXAMPLE "It might numb your pain in your hand")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

