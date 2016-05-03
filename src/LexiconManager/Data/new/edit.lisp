;;;;
;;;; W::edit
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::edit
  (wordfeats (W::morph (:forms (-vb) :past W::edited :ing W::editing)))
   (SENSES
    ((EXAMPLE "edit the text")
     (LF-PARENT ONT::revise)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

