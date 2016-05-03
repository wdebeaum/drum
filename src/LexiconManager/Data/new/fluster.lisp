;;;;
;;;; W::fluster
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::fluster
   (wordfeats (W::morph (:forms (-vb) :past W::flustered :ing W::flustering)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("fluster%2:37:00" "fluster%2:37:01"))
     (LF-PARENT ONT::evoke-confusion)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

