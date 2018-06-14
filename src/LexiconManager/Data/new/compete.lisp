;;;;
;;;; W::COMPETE
;;;;


(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
   (W::compete
     (wordfeats (W::morph (:forms (-vb) :nom W::competition)))
   (SENSES
    (
     ;(LF-PARENT ONT::fighting)
     (LF-PARENT ONT::compete)
     (TEMPL agent-templ) ; like stroll,walk
     )
    (
     (LF-PARENT ONT::compete)
     (TEMPL agent-affected-as-comp-TEMPL (xp (% W::pp (W::ptype (? xxx W::for W::over)))))
     (example "They competed for the prize.")
     )
    )
   )
))

