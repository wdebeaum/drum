;;;;
;;;; W::shorten
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::shorten
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050919 :change-date 20090504 :comments nil)
     (EXAMPLE "She shortened the dress.")
     (LF-PARENT ONT::shorten)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (f::scale ont::length-scale))
     )
    ((meta-data :origin task-learning :entry-date 20050919 :change-date 20090504 :comments nil)
     (EXAMPLE "The tail shortened over time.")
     (LF-PARENT ONT::shorten)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (f::scale ont::length-scale))
     (TEMPL affected-unaccusative-templ)
     )    
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::shorten
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "He shortened our meeting")
     (LF-PARENT ONT::shorten-time)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic) (F::scale ont::duration-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "our meeting shortened (by 10 min)")
     (LF-PARENT ONT::shorten-time)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::scale ont::duration-scale))
     (TEMPL affected-unaccusative-templ)
     )
))))

