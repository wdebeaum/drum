;;;;
;;;; w::lighten
;;;;


(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::lighten
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "He lightened the load.")
     (LF-PARENT ONT::lighten-weight)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::weight-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "The load lightened quickly.")
     (LF-PARENT ONT::lighten-weight)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::weight-scale))
     (TEMPL affected-unaccusative-templ)
     )
))))