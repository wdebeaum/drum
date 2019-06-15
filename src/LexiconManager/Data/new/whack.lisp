;;;;
;;;; W::whack
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::whack
    (wordfeats (W::morph (:forms (-vb) :nom W::whack)))
   (SENSES
    ((LF-PARENT ONT::hitting)
     (example "the man whacked the ball")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin cardiac :entry-date 20081223 :change-date nil :comments LM-vocab)
     )
    )
   )
))

