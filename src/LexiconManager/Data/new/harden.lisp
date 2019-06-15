;;;; 
;;;; w::harden
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::harden
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "harden the clay")
     (LF-PARENT ONT::harden)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::tactile-hardness-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "the clay hardened")
     (LF-PARENT ONT::harden)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::tactile-hardness-scale))
     (TEMPL affected-unaccusative-templ)
     )
))))
