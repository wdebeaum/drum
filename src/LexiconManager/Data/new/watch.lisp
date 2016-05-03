;;;;
;;;; W::watch
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
;; seems like this should be an agent instead of experiencer, but ont::active-perception also seems good
 (W::watch
   (SENSES
    ((LF-PARENT ONT::scrutiny)
     (example "watch it")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-NEUTRAL-TEMPL)
     )
    ((lf-parent  ont::manage) ;; 20120521 GUM change new parent 
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he is watching his weight")
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     )
    )
   )
))

