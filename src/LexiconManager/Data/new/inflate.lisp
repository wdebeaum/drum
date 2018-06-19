;;;;
;;;; w::inflate
;;;;

(define-words :pos W::v 
  :words (
  (w::inflate
   (wordfeats (W::morph (:forms (-vb) :nom w::inflation)))
   (senses
    (
     (LF-PARENT ONT::increase)
     (example "I inflated the ballon")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     )
    (
     (LF-PARENT ONT::increase)
     (example "The balloon inflated.")
     (templ affected-templ)
     )
    )
 )
))

