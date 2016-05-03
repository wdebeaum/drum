;;;;
;;;; W::announce
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::announce
   (wordfeats (W::morph (:forms (-vb) :nom w::announcement)))
   (SENSES
    ((EXAMPLE "apple announced the new imac")
     (meta-data :origin calo :entry-date 20040915 :change-date 20090506 :comments caloy2)
     (LF-PARENT ONT::tell)
     (example "announce it to them")
     ;; this verb doesn't participate in the alternation
     (TEMPL agent-theme-to-addressee-optional-templ) 
     )
    ((LF-PARENT ONT::tell)
     (example "microsoft announced yesterday that they will acquire yahoo")
     (meta-data :origin joshua :entry-date 20080905 :change-date 20090506 :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-to W::s-finite)))))
     )
    )
    )
   )
)

