;;;;
;;;; W::reach
;;;;

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :tags (:base500)
 :words (
;   )
  (W::reach
   (SENSES
    ((LF-PARENT ONT::REACH)
     (example "he reached a conclusion") ;; not a goal
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-neutral-XP-TEMPL (xp (% W::np)))
     )
    )
   ) 
))

