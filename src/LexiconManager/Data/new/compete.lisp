;;;;
;;;; W::COMPETE
;;;;


(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
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
     (TEMPL AGENT-AFFECTED-XP-PP-TEMPL (xp (% W::pp (W::ptype (? xxx W::for W::over)))))
     (example "They competed for the prize.")
     )
    )
   )
))

