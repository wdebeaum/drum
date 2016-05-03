;;;;
;;;; W::offend
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::offend
   (wordfeats (W::morph (:forms (-vb) :nom W::offense)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090520 :comments nil :vn ("amuse-31.1") :wn ("offend%2:37:00" "offend%2:37:01" "offend%2:37:02"))
     (LF-PARENT ONT::evoke-offense)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

