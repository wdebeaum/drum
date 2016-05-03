;;;;
;;;; W::define
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::define
    (wordfeats (W::morph (:forms (-vb) :nom w::definition)))
   (SENSES
     ((LF-PARENT ONT::classify)
     (meta-data :origin lou :entry-date 20041116 :change-date 20090501 :comments lou-demo)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "define this as a compound")
     (TEMPL agent-neutral-as-theme-templ)
     )
    ;; need this definition to allow v-passive-by rule to apply; comp arg must be -
    ((LF-PARENT ONT::classify)
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090501 :comments caloy3)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "define this" "this was defined by him")
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))

