;;;;
;;;; W::support
;;;;

(define-words :pos W::V 
 :words (
  (W::support
   (wordfeats (W::morph (:forms (-vb) :nom W::support)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("admire-31.2") :wn ("support%2:31:04" "support%2:41:00" "support%2:41:01"))
     (LF-PARENT ONT::CONFIRM)
     (TEMPL agent-neutral-xp-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     (EXAMPLE "He supports the petition")
     )
    ;; the foundation supports the building

    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("admire-31.2") :wn ("support%2:31:04" "support%2:41:00" "support%2:41:01"))
     (LF-PARENT ONT::CONFIRM)
     (TEMPL AGENT-FORMAL-XP-CP-TEMPL) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     (EXAMPLE "He supports the cat releases the mouse.")
     )
    (
     (LF-PARENT ONT::CORRELATION)
     (example "The result supported the hypothesis")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    (
     (LF-PARENT ONT::CORRELATION)
     (example "The result supported that the cat ate the mouse.")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )

    )
   )
))

