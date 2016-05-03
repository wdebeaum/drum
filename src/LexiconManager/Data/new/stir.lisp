;;;;
;;;; W::stir
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::stir
   (wordfeats (W::morph (:forms (-vb) :nom W::stir)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("stir%2:36:00" "stir%2:37:01"))
     (LF-PARENT ONT::evoke-excitement)
     (TEMPL agent-affected-xp-templ)
     )
    ((meta-data :origin caet :entry-date 20120128 :comments g3)
     (LF-PARENT ONT::stir)
     (example "the wind stirred the leaves" "the cook stirred the broth")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  ((W::stir w::fry)
   (wordfeats (W::morph (:forms (-vb) :past (W::stir w::fried) :ing( W::stir w::frying) )))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::cooking) ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))

