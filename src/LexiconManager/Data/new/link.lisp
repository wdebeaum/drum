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

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::link
   (SENSES
    ;;;; link the car to the train
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    ;;;; the path links one city to/with another
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level))
     (TEMPL neutral-neutral-xp-templ)
     )
    ;;;; the path links 2 cities
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level))
     (TEMPL neutral-neutral-plural-templ)
     )
    )
   )
))

