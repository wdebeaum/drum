;;;;
;;;; W::BEND
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BEND
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::CORNER)
     (TEMPL PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::bend
   (wordfeats (W::morph (:forms (-vb) :past W::bent :ing W::bending)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("assuming_position-50") :wn ("bend%2:38:03"))
     (LF-PARENT ONT::body-movement-place)
     
     (TEMPL agent-templ) ; like lie
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("bend-45.2") :wn ("?bend%2:38:00" "bend%2:35:00"))
     (LF-PARENT ONT::fold)
 ; like fold1
     )
    )
   )
))

