;;;;
;;;; W::bewilder
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::bewilder
   (wordfeats (W::morph (:forms (-vb) :past W::bewildered :ing W::bewildering)))
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("bewilder%2:31:00" "bewilder%2:37:00"))
     (LF-PARENT ONT::evoke-confusion)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     )
    )
   )
))

