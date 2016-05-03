;;;;
;;;; W::concentrate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::concentrate
   (SENSES
    ((LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "concentrate on the problem")
     (TEMPL agent-neutral-xp-templ (xp (% W::pp (W::ptype W::on))))
     (meta-data :origin calo :entry-date 20050308 :change-date nil :comments computer-purchasing :vn ("focus-87"))
     )
    )
   )
))

