;;;;
;;;; W::iterate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::iterate
     (wordfeats (W::morph (:forms (-vb) :nom W::iteration)))
   (SENSES
    ;;;; repeat the statement/the information
    ((LF-PARENT ONT::REPEAT)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

