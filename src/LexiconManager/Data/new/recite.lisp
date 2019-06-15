;;;;
;;;; W::recite
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::recite
   (wordfeats (W::morph (:forms (-vb) :nom W::recitation)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("transfer_mesg-37.1"))
     (LF-PARENT ONT::scripted-say)
     ;(TEMPL agent-affected-iobj-theme-templ) ; like relay
     (TEMPL AGENT-NEUTRAL-AFFECTED-TEMPL)
     )
    )
   )
))

