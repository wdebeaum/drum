;;;;
;;;; W::kill
;;;;

(define-words :pos W::v
 :words (
 (W::kill
   (SENSES
    ((LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "kill the program")
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((LF-PARENT ONT::kill)
     (example "He killed the rabbit")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((LF-PARENT ONT::kill)
     (TEMPL agent-templ)
     (example "Poison kills.")
     ))
   )))

