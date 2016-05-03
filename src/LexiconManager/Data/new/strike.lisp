;;;;
;;;; W::strike
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::strike
   (wordfeats (W::morph (:forms (-vb) :past W::struck :nom w::strike)))
   (SENSES
    ((LF-PARENT ONT::hitting)
     (example "a truck struck a train")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-XP-TEMPL)
     (meta-data :origin plot :entry-date 2080413)
     )
    (
;     (LF-PARENT ONT::type)
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype)
     (example "strike the key")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-XP-TEMPL)
     (meta-data :origin plot :entry-date 2080413)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("strike%2:37:00"))
     (LF-PARENT ONT::evoke-excitement)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

