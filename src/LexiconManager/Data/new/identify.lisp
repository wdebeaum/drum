;;;;
;;;; W::identify
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::identify
   (SENSES
    #|((LF-PARENT ONT::relate)
     (example "he related it to/with that")
     ;(TEMPL agent-affected2-templ (xp (% w::pp (w::ptype w::with)))) ; like affiliate,confederate,muddle,entangle,confuse,associate,team,pair
     (TEMPL neutral-neutral-neutral-xp-templ (xp (% W::pp (W::ptype(? ptype W::with W::to)))))
     (PREFERENCE 0.96)
     )|#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090501 :comments nil :vn ("characterize-29.2-1-1"))
     (LF-PARENT ONT::classify)
     (TEMPL AGENT-FORMAL-XP-TEMPL) ; like represent
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::classify)
     (meta-data :origin lou :entry-date 20040716 :change-date 20041116 :comments lou-demo)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "identify this (as a mine)")
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-OPTIONAL-TEMPL)
     )
    ;; need this definition to allow v-passive-by rule to apply; comp arg must be -
    ((LF-PARENT ONT::classify)
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090501 :comments caloy3)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "this was identified by him")
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

