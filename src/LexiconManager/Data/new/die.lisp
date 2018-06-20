;;;;
;;;; W::die
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::die
   (wordfeats (W::morph (:forms (-vb) :past W::died :ing w::dying :nom w::death)))
    (SENSES
    ((LF-PARENT ONT::die)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-cause-optional-xp-TEMPL (xp (% w::pp (w::ptype (? xx w::from w::by)))))
     )
    )
   )
  ))

