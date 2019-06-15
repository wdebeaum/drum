;;;;
;;;; w::trim
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(w::trim
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090601 :comments nil)
   (LF-PARENT ONT::remove-from)
   (example "trim the fat [from the meat]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL)
   )
  ((meta-data :origin "wordnet-3.0" :entry-date 20090601 :change-date nil :comments nil)
   (LF-PARENT ONT::remove-from)
   (example "trim the tree [of branches]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-OF-OPTIONAL-TEMPL)
   )
  )
 )
))

