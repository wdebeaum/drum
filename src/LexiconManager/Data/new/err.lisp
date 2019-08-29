;;;;
;;;; W::err
;;;;

(define-words :pos W::v
 :words (
   (W::err
   (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((LF-PARENT ONT::err)
      (example "he erred")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-TEMPL)
    )
   ))
))



