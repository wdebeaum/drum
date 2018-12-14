;;;;
;;;; W::laugh
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
(W::laugh
   (SENSES
    ((LF-PARENT ONT::SOUND-EXPRESSION)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (EXAMPLE "She laughed loudly.")
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::laugh w::out w::loud)
   (SENSES
    ((LF (W::GOODBYE))
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_expressive))
     )
    )
   )
))

