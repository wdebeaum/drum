;;;;
;;;; W::savor
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::savor
   (wordfeats (W::morph (:forms (-vb) :past W::savored :ing W::savoring)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("admire-31.2") :wn ("savor%2:37:00"))
     (LF-PARENT ONT::appreciate)
     (TEMPL experiencer-neutral-xp-templ) ; like worship,treasure,venerate,appreciate,prize,value
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("sight-30.2") :wn ("savor%2:37:00" "savor%2:39:00"))
     (LF-PARENT ONT::active-perception)
     (TEMPL experiencer-neutral-templ) ; like observe,view,watch
     )
    ;((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("admire-31.2") :wn ("savor%2:37:00"))
     ;(LF-PARENT ONT::experiencer-emotion)
     ;(TEMPL agent-neutral-xp-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
;     )
    )
   )
))

