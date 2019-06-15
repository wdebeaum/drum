;;;;
;;;; W::mourn
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::mourn
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("admire-31.2") :wn ("mourn%2:37:00"))
     (LF-PARENT ONT::cogitation)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL) ; like suffer
     (EXAMPLE "I mourned him singing.")
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("admire-31.2") :wn ("mourn%2:37:00"))
     (LF-PARENT ONT::cogitation)
     (TEMPL AGENT-FORMAL-XP-TEMPL) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     )
    )
   )
))

