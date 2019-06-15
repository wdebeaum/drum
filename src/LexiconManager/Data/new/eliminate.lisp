;;;;
;;;; W::eliminate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::eliminate
   (SENSES
    ((LF-PARENT ONT::discard)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::from))))
     (EXAMPLE "eliminate the smtp server")
     (meta-data :origin task-learning :entry-date 20050916 :change-date 20090529 :comments nil :vn ("remove-10.1") :wn ("eliminate%2:29:00" "eliminate%2:29:01" "eliminate%2:30:01" "eliminate%2:31:00" "eliminate%2:42:01"))
     )
    )
   )
))

