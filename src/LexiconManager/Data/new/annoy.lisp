;;;;
;;;; W::annoy
;;;;


(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::annoy
   (wordfeats (W::morph (:forms (-vb) :nom w::annoyance)))
   (SENSES
    ((EXAMPLE "he annoys her")
     (LF-PARENT ONT::evoke-annoyance)
     (meta-data :origin calo :entry-date 20040915 :change-date 20090512 :comments caloy2)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

