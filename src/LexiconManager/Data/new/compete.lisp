;;;;
;;;; W::COMPETE
;;;;


(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
   (W::race
     (wordfeats (W::morph (:forms (-vb) :nom W::competition)))
   (SENSES
    (
     (LF-PARENT ONT::fighting)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

