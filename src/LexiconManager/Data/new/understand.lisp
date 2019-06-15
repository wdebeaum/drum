;;;;
;;;; W::understand
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::understand
   (wordfeats (W::morph (:forms (-vb) :past W::understood)))
   (SENSES
    ((LF-PARENT ONT::understand)
      (meta-data :origin "trips" :entry-date 20060315 :change-date nil :comments nil :wn ("understand%2:31:00" "understand%2:31:01"))
     (example "I understand the plan")
     (TEMPL experiencer-theme-xp-templ)
     )
    ((LF-PARENT ONT::KNOW)
     (TEMPL experiencer-theme-xp-templ (xp (% W::cp (w::ctype w::s-that) (w::wh w::-))))
     )
    ((LF-PARENT ONT::understand)
     (SEM (F::Aspect F::Indiv-level))
     (TEMPL experiencer-templ)
     (example "I understand")
     )
    )
   )
))

