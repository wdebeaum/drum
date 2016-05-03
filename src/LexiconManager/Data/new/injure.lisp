;;;;
;;;; W::injure
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::injure ; like hurt
    (wordfeats (W::morph (:forms (-vb) :past W::injured :ing W::injuring :nom w::injury)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date 20090512 :comments nil :vn ("hurt-40.8.3-2") :wn ("injure%2:29:00"))
     (LF-PARENT ONT::evoke-injury)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

