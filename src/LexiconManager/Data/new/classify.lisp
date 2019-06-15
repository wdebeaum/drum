;;;;
;;;; W::classify
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::classify
   (SENSES
    ((LF-PARENT ONT::classify)
     (meta-data :origin lou :entry-date 20041116 :change-date 20090501 :comments lou-demo :vn ("characterize-29.2") :wn ("classify%2:31:00" "classify%2:31:01"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "classify this as a mine")
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-TEMPL)
     )
    ;; need this definition to allow v-passive-by rule to apply; comp arg must be -
    ((LF-PARENT ONT::classify)
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090501 :comments caloy3)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "this was classified by him")
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

