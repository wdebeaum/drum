;;;;
;;;; W::stretch
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::stretch
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("stretch%1:28:00"))
     (LF-PARENT ONT::time-interval)
     (example "they waited a long stretch")
     (TEMPL other-reln-templ)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
   (W::stretch
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil)
     ;;(LF-PARENT ONT::body-movement)
     (lf-parent ont::stretch) ;; 20120523 GUM change new parent
     (TEMPL affected-templ)
     (preference .98)
     )
    ((meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil)
     ;;(LF-PARENT ONT::body-movement)
     (lf-parent ont::stretch) ;; 20120523 GUM change new parent
     (example "he stretched his legs")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::stretch
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "She stretched the fabric.")
     (LF-PARENT ONT::extend)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::area-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "The fabric stretched in the wash.")
     (LF-PARENT ONT::extend)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::area-scale))
     (TEMPL affected-unaccusative-templ)
     )
))))

