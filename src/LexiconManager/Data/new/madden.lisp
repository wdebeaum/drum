;;;;
;;;; W::madden
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::madden
   (wordfeats (W::morph (:forms (-vb) :past W::maddened :ing W::maddening)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("madden%2:37:00" "madden%2:37:01"))
     (LF-PARENT ONT::evoke-anger)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

