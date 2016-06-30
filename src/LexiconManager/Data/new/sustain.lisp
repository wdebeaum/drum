;;;;
;;;; W::sustain
;;;;

(define-words :pos W::V 
 :words (
  (W::sustain
    (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    (
     (LF-PARENT ONT::activity-ongoing)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))
