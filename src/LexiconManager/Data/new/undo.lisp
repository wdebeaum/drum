;;;;
;;;; W::undo
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::undo
   (wordfeats (W::morph (:forms (-vb) :3s W::undoes :past W::undid)))
   (SENSES
    ((LF-PARENT ONT::UNDO)
     (example "undo the action")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL AGENT-EFFECT-OPTIONAL-TEMPL)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((LF-PARENT ONT::UNDO)
     (example "undo")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-templ)
     )
    )
   )
))

