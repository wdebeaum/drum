;;;; 
;;;; w::lengthen
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::lengthen
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "lengthen the skirt")
     (LF-PARENT ONT::lengthen)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::length-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "The shadows lengthened quickly")
     (LF-PARENT ONT::lengthen)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::length-scale))
     (TEMPL affected-unaccusative-templ)
     )
))))


(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::lengthen
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "He lengthened our meeting")
     (LF-PARENT ONT::extend-time)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic) (F::scale ont::duration-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "our meeting lengthened (by 10 min)")
     (LF-PARENT ONT::extend-time)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::scale ont::duration-scale))
     (TEMPL affected-unaccusative-templ)
     )
))))