;;;;
;;;; W::resent
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::resent
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("admire-31.2") :wn ("resent%2:37:01"))
     (LF-PARENT ONT::disliking)
     ;(TEMPL experiencer-action-objcontrol-templ) ; like suffer
     (TEMPL EXPERIENCER-FORMAL-SUBJCONTROL-TEMPL  (xp (% W::VP (W::vform W::ing))))
     (EXAMPLE "I resent talking to him")
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("admire-31.2") :wn ("resent%2:37:01"))
     (LF-PARENT ONT::disliking)
     (TEMPL experiencer-neutral-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     )
    )
   )
))

