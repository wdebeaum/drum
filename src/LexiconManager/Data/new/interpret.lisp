;;;;
;;;; W::interpret
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::interpret
 (wordfeats (W::morph (:forms (-vb) :nom w::interpretation)))
   (SENSES
    ((LF-PARENT ONT::classify)
     (meta-data :origin calo-ontology :entry-date 20051209 :change-date 20090501 :comments Interpret :vn ("characterize-29.2"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "I interpret this message as a warning")
     (TEMPL agent-neutral-as-theme-templ)
     )
    ;; need this definition to allow v-passive-by rule to apply; comp arg must be -
    ((LF-PARENT ONT::classify)
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090501 :comments caloy3)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "this was interpreted by him")
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))

