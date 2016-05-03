;;;;
;;;; W::frighten
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::frighten
   (wordfeats (W::morph (:forms (-vb) :past W::frightened :ing W::frightening)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("frighten%2:37:00"))
     (LF-PARENT ONT::evoke-fear)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

