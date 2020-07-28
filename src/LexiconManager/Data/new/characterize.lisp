;;;;
;;;; W::characterize
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::characterize
  (wordfeats (W::morph (:forms (-vb) :nom w::characterization)))
   (SENSES
    ((LF-PARENT ONT::categorization)
     (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil :vn ("characterize-29.2"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he characterized the situation (as positive)")
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-TEMPL)
     )
    ;; need this definition to allow v-passive-by rule to apply; comp arg must be -
    ((LF-PARENT ONT::categorization)
     (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil :vn ("characterize-29.2"))
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "this was characterized by him")
     (TEMPL AGENT-NEUTRAL-XP-TEMPL)
     )
    ((LF-PARENT ONT::is-compatible-with)
     ;; changed templ to theme-cotheme so passive rule will work
     (meta-data :origin step :entry-date 20080630 :change-date 20080826 :comments nil :vn ("characterize-29.2"))
      (SEM (F::Time-span F::extended))
     (example "limited exposure characterizes normal usage")
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    )
   )
))

