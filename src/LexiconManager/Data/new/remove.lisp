;;;;
;;;; W::remove
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::remove
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090602 :comments nil :vn ("banish-10.2") :wn ("remove%2:30:00" "remove%2:38:00" "remove%2:40:00" "remove%2:41:00" "remove%2:41:02"))
     (LF-PARENT ONT::cause-come-from)
     (example "remove the cargo from the truck")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::from))))
     )
    )
   )
))

