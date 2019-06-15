;;;;
;;;; W::die
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::die
   (wordfeats (W::morph (:forms (-vb) :past W::died :ing w::dying :nom w::death)))
    (SENSES
    ((LF-PARENT ONT::die)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AFFECTED-AGENT-XP-PP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? xx w::from w::by)))))
     )
    )
   )
  ))

