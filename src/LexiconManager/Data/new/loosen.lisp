;;;; 
;;;; w::loosen
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::loosen
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "loosen the screw")
     (LF-PARENT ONT::loosen)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::looseness-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "the screw loosened")
     (LF-PARENT ONT::loosen)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::looseness-scale))
     (TEMPL affected-unaccusative-templ)
     )
))))
