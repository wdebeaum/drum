;;;;
;;;; W::wait
;;;;

(define-words :pos W::v 
 :tags (:base500)
 :words (
;   )
  (W::wait
   (SENSES
    ((LF-PARENT ONT::WAIT-WATCH)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     (example "wait five minutes")
     )
    ((lf-parent ont::wait-watch)
     (example "He is waiting for the boat to arrive")
     (TEMPL agent-action-templ)
     )

    ((lf-parent ont::wait-watch)
     (example "He is waiting for the boat")
     (TEMPL agent-neutral-xp-templ (xp (% W::PP (W::ptype w::for)))
     )
   )
))))

