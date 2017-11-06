;;;;
;;;; W::sort
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::sort
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((EXAMPLE "What sort of networking options are available?")
     (meta-data :origin calo :entry-date 20040504 :change-date nil :wn ("sort%1:09:00") :comments calo-y1variants)
     (LF-PARENT ONT::KIND)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::sort
   (SENSES
    ((lf-parent ont::sort)
     (example "sort them into groups of three")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::into))))
     (meta-data :origin plow :entry-date 20060626 :change-date 20090505 :comments pq0380)
     )
    ((lf-parent ont::sort)
     (example "sort them by departure date")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-theme-TEMPL (xp (% W::PP (W::ptype W::by))))
     (meta-data :origin plow :entry-date 20070928 :change-date 20090505 :comments plow-travel)
     )
    ((lf-parent ont::sort)
     (example "sort by departure date")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-theme-xp-TEMPL (xp (% W::PP (W::ptype W::by))))
     (meta-data :origin plow :entry-date 20071003 :change-date 20090505 :comments plow-travel)
     )
    ;; need this for passive
    ((lf-parent ont::sort)
     (example "sort them into groups of three")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin plow :entry-date 20060626 :change-date 20090505 :comments pq0380)
     )

    #| ; moved to domain-words
    ((lf-parent ont::TRANSPORT)
     (example "protein sorting")
     (TEMPL agent-affected-xp-templ)
     )
    |#

    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::sort W::of)
   (SENSES

    ((LF-PARENT ONT::DEGREE-OF-BELIEF)
     (LF-FORM W::SORT-OF)
     (TEMPL PRED-S-VP-TEMPL)
     )
    ((LF-PARENT ONT::DEGREE-MODIFIER-MED)
     (LF-FORM W::SORT-OF)
     (TEMPL ADJ-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::DEGREE-MODIFIER-MED)
     (LF-FORM W::SORT-OF)
     (TEMPL PRED-VP-PRE-TEMPL)
     )
    )
   )
))

