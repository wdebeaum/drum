;;;;
;;;; W::threaten
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::threaten
   (wordfeats (W::morph (:forms (-vb) :past W::threatened :ing W::threatening)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("threaten%2:42:00"))
     (LF-PARENT ONT::evoke-fear)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

