;;;;
;;;; W::cite
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::cite
    (wordfeats (W::morph (:forms (-vb) :nom w::citation)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("transfer_mesg-37.1"))
     (LF-PARENT ONT::scripted-say)
     ;(TEMPL agent-affected-iobj-theme-templ) ; like relay
     (TEMPL agent-affected-iobj-neutral-templ)
     )
    )
   )
))

