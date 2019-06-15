;;;;
;;;; W::link
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::link
     (SENSES
      ((EXAMPLE "click on the link to my homepage")
      (meta-data :origin plow :entry-date 20050318 :change-date nil :comments nil)
      (LF-PARENT ONT::link)
      )
     )
    )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::link
   (SENSES
    ;;;; link the car to the train
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% W::pp (w::ptype (? pt W::to w::with)))))
     )
    ;;;; the path links one city to/with another
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::pp (W::ptype (? pt W::to w::with)))) )
     )
    ;;;; the path links 2 cities
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level))
     (TEMPL NEUTRAL-NEUTRAL1-NP-PLURAL-TEMPL)
     )
    )
   )
))

