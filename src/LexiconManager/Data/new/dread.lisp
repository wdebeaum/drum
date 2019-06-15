;;;;
;;;; W::dread
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::dread
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("admire-31.2") :wn ("dread%2:37:00"))
     (LF-PARENT ONT::fearing)
     ;(TEMPL experiencer-action-objcontrol-templ) ; like suffer
     (TEMPL EXPERIENCER-FORMAL-SUBJCONTROL-TEMPL  (xp (% W::VP (W::vform W::ing))))
     ;(TEMPL experiencer-theme-SUBJCONTROL-TEMPL)
     (EXAMPLE "I dread going to the dentist")
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("admire-31.2") :wn ("dread%2:37:00"))
     (LF-PARENT ONT::fearing)
     (TEMPL experiencer-neutral-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     )
    )
   )
))

