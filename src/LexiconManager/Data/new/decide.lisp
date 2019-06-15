;;;;
;;;; W::decide
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::decide
   (wordfeats (W::morph (:forms (-vb) :nom w::decision)))
   (SENSES
    ((LF-PARENT ONT::decide)
     (example "decide how to do it")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-if))))
     )
    ((LF-PARENT ONT::decide)
     (example "he decided to do it")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL)
     )
    ((LF-PARENT ONT::decide)
     (example "decide on the plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::pp (W::ptype W::on))))
     )
    ((LF-PARENT ONT::decide)
     (example "decide the issue")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
     ((LF-PARENT ONT::decide)
     (example "the committee decided this morning")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-templ)
     (preference. 96)
     )
    )
   )
))

