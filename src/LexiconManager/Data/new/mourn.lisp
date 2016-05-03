;;;;
;;;; W::mourn
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::mourn
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("admire-31.2") :wn ("mourn%2:37:00"))
     (LF-PARENT ONT::cogitation)
     (TEMPL agent-theme-objcontrol-templ) ; like suffer
     (EXAMPLE "I mourned him singing.")
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("admire-31.2") :wn ("mourn%2:37:00"))
     (LF-PARENT ONT::cogitation)
     (TEMPL agent-theme-xp-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     )
    )
   )
))

