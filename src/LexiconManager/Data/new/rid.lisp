;;;;
;;;; W::rid
;;;;

(define-words :pos W::v 
 :words (
 (W::rid
   (wordfeats (W::morph (:forms (-vb) :past W::rid)))
   (SENSES
    ((LF-PARENT ONT::remove-from)
     (example "rid him of it")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-source-affected-optional-templ)
     )
    )
   )
))

