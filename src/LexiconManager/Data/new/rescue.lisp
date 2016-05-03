;;;;
;;;; W::rescue
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::rescue
   (wordfeats (W::morph (:forms (-vb) :nom W::rescue)))
   (SENSES
    ;;;; swier -- rescue the people
    ((LF-PARENT ONT::rescue)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

