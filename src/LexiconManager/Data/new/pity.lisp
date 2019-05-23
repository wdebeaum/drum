;;;;
;;;; W::pity
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::pity
    (wordfeats (W::morph (:forms (-vb) :nom W::pity)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("admire-31.2") :wn ("pity%2:37:00"))
     (LF-PARENT ONT::pity)
     (TEMPL experiencer-action-objcontrol-templ) ; like suffer
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("admire-31.2") :wn ("pity%2:37:00"))
     (LF-PARENT ONT::pity)
     (TEMPL experiencer-neutral-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     )
    )
   )
))

