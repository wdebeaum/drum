;;;;
;;;; W::sadden
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::sadden
   (wordfeats (W::morph (:forms (-vb) :past W::saddened :ing W::saddening)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("sadden%2:37:01"))
     (LF-PARENT ONT::evoke-sadness)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

