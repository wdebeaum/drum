;;;;
;;;; W::bother
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::bother
    (wordfeats (W::morph (:forms (-vb) :past W::bothered :ing W::bothering)))
   (SENSES
    ((EXAMPLE "light bothers his eyes")
     (LF-PARENT ONT::evoke-bother)
     (meta-data :origin calo :entry-date 20040915 :change-date 20090512 :comments caloy2)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

