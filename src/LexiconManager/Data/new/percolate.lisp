;;;;
;;;; W::percolate
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::percolate
    (wordfeats (W::morph (:forms (-vb) :nom W::percolation)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3") :wn ("percolate%2:35:03"))
     (LF-PARENT ONT::cooking)
 ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))

