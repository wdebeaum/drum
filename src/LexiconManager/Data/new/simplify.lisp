;;;;
;;;; W::simplify
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::simplify
   (wordfeats (W::morph (:forms (-vb) :nom w::simplification)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date 20090504 :comments html-purchasing-corpus)
     (LF-PARENT ONT::language-adjust)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

