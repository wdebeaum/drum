;;;;
;;;; W::pull
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::pull
   (SENSES
    ;;;; swier -- We can pull one more boxcar.
    ((LF-PARENT ONT::PULL)
     (SEM (F::cause F::agentive) (F::aspect F::unbounded) (F::time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-OPTIONAL-B-TEMPL)
     )
    ((LF-PARENT ONT::pull)
     (SEM (F::cause F::agentive) (F::aspect F::bounded) (F::time-span F::extended))
     (TEMPL affected-SOURCE-xp-TEMPL)
     (example "The plug pulled out from the wall")
     )
    ))))

#||(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::pull (W::out))
   (SENSES
    #||((LF-PARENT ONT::pull-out-of)
     (LF-FORM W::pull-out)
     (SEM (F::cause F::agentive) (F::aspect F::bounded) (F::time-span F::extended))
     (TEMPL AGENT-affected-SOURCE-OPTIONAL-TEMPL (xp (% W::pp (W::ptype (? ptype W::of W::from)))))
     )||#
    ((LF-PARENT ONT::pull-out-of)
     (LF-FORM W::pull-out)
     (SEM (F::cause F::agentive) (F::aspect F::bounded) (F::time-span F::extended))
     (TEMPL affected-SOURCE-xp-TEMPL)
     (example "The plug pulled out from the wall")
     )
    )
   )
))||#

#||(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::pull (W::off))
   (SENSES
    ((LF-PARENT ONT::pull-off)
     (LF-FORM W::pull-off)
     (SEM (F::cause F::agentive) (F::aspect F::bounded) (F::time-span F::extended))
     (TEMPL AGENT-affected-SOURCE-OPTIONAL-TEMPL (xp (% W::pp (W::ptype (? ptype W::of W::from)))))
     )
    )
   )
))||#

