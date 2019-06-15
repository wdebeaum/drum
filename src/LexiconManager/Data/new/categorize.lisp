;;;;
;;;; W::categorize
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::categorize
  (wordfeats (W::morph (:forms (-vb) :nom w::categorization)))
   (SENSES
    ((LF-PARENT ONT::classify)
     (meta-data :origin step :entry-date 20080630 :change-date 20090501 :comments nil :vn ("characterize-29.2"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he categorized the situation (as positive)")
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-TEMPL)
     )
    ;; need this definition to allow v-passive-by rule to apply; comp arg must be -
    ((LF-PARENT ONT::classify)
     (meta-data :origin step :entry-date 20080630 :change-date 20090501 :comments nil :vn ("characterize-29.2"))
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "this was categorized by him")
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

