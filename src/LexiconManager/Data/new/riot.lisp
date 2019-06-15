;;;;
;;;; W::RIOT
;;;;

#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::RIOT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("riot%1:04:00"))
     (LF-PARENT ONT::located-EVENT)
     )
    )
   )
))
|#

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::RIOT
   (wordfeats (W::morph (:forms (-vb) :ing W::rioting :nom w::riot)))
   (SENSES
    (;(LF-PARENT ONT::RIOT)
     (LF-PARENT ONT::GROUP-CONFLICT)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

