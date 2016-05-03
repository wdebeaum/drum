;;;;
;;;; W::punish
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::punish
   (wordfeats (W::morph (:forms (-vb) :nom w::punishment)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090513 :comments nil :vn ("judgement-33") :wn ("punish%2:41:00"))
     (LF-PARENT ont::punish)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

