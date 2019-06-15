;;;;
;;;; W::favor
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::favor
   (wordfeats (W::morph (:forms (-vb) :past W::favored :ing W::favoring)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("admire-31.2") :wn ("favor%2:41:00" "favor%2:41:01" "favor%2:41:03"))
     (LF-PARENT ONT::appreciate)
     (TEMPL experiencer-neutral-xp-templ) ; like worship,treasure,venerate,appreciate,prize,value
     )
    ;((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("admire-31.2") :wn ("favor%2:41:00" "favor%2:41:01" "favor%2:41:03"))
     ;(LF-PARENT ONT::experiencer-emotion)
     ;(TEMPL agent-neutral-xp-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
;     )
    (
     (LF-PARENT ONT::cause-stimulate)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     )

    )
   )
))

