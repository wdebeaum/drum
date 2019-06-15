;;;;
;;;; W::COUPLE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::COUPLE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20090520 :comments nil :wn ("couple%1:23:00"))
     (LF-PARENT ONT::quantity)
     (example "they are a couple")
     (TEMPL classifier-count-pl-templ)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::COUPLE
   (SENSES
    ((LF-PARENT ONT::ATTACH)
     (example "couple the car with the train")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype (? pt w::to W::with)))))
     )
    )
   )
))

