;;;;
;;;; w::trim
;;;;

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
(w::trim
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090601 :comments nil)
   (LF-PARENT ONT::remove-from)
   (example "trim the fat [from the meat]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-SOURCE-OPTIONAL-TEMPL)
   )
  ((meta-data :origin "wordnet-3.0" :entry-date 20090601 :change-date nil :comments nil)
   (LF-PARENT ONT::remove-from)
   (example "trim the tree [of branches]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-SOURCE-affected-OPTIONAL-TEMPL)
   )
  )
 )
))

