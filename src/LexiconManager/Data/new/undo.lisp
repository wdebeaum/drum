;;;;
;;;; W::undo
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::undo
   (wordfeats (W::morph (:forms (-vb) :3s W::undoes :past W::undid)))
   (SENSES
    ((LF-PARENT ONT::UNDO)
     (example "undo the action")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-EFFECT-OPTIONAL-TEMPL)
     )
    )
   )
))

