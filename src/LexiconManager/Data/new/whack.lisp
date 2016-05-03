;;;;
;;;; W::whack
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::whack
    (wordfeats (W::morph (:forms (-vb) :nom W::whack)))
   (SENSES
    ((LF-PARENT ONT::hitting)
     (example "the man whacked the ball")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-XP-TEMPL)
     (meta-data :origin cardiac :entry-date 20081223 :change-date nil :comments LM-vocab)
     )
    )
   )
))

