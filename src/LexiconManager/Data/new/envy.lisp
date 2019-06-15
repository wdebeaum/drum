;;;;
;;;; W::envy
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::envy
   (SENSES
    ;((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("admire-31.2") :wn ("envy%2:37:00" "envy%2:37:01"))
     ;(LF-PARENT ONT::envying)
     ;(TEMPL experiencer-action-objcontrol-templ) ; like suffer
;     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("admire-31.2") :wn ("envy%2:37:00" "envy%2:37:01"))
     (LF-PARENT ONT::envying)
     (TEMPL experiencer-neutral-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     )
    )
   )
))

