;;;;
;;;; W::penalize
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::penalize
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("penalize%2:41:00"))
     (LF-PARENT ont::punish)
     (TEMPL agent-affected-xp-templ)
     )
     ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::punish)
     (example "he penalized it")
     (TEMPL agent-theme-xp-templ) 
     )
    )
   )
))

