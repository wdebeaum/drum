;;;;
;;;; W::sicken
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::sicken
   (wordfeats (W::morph (:forms (-vb) :past W::sickened :ing W::sickening)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("sicken%2:37:00"))
     (LF-PARENT ONT::evoke-disgust)
     (TEMPL agent-affected-xp-templ)
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
     (LF-PARENT ONT::evoke-ill-being)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

