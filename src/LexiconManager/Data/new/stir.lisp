;;;;
;;;; W::stir
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::stir
   (wordfeats (W::morph (:forms (-vb) :nom W::stir)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("stir%2:36:00" "stir%2:37:01"))
     (LF-PARENT ONT::evoke-excitement)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((meta-data :origin caet :entry-date 20120128 :comments g3)
     (LF-PARENT ONT::stir)
     (example "the wind stirred the leaves" "the cook stirred the broth")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::stir w::fry)
   (wordfeats (W::morph (:forms (-vb) :past (W::stir w::fried) :ing( W::stir w::frying) )))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::cook-in-fat) ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))

