;;;;
;;;; w::meet
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((w::meet w::up)
    (wordfeats (W::morph (:forms (-vb) :past w::met :nom (w::meet w::up))))
   (senses
    ((meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3 :vn ("herd-47.5.2") :wn ("congregate%2:38:00"))
     (LF-PARENT ONT::meet)
     (example "they met up at the church")
     (TEMPL agent-plural-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::meet
   (wordfeats (W::morph (:forms (-vb) :past W::met)))
   (SENSES
    ;;;; he met a friend
    ((LF-PARENT ONT::meet)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-co-agent-xp-TEMPL)
     )
    ;;;;he met with a friend
    ((LF-PARENT ONT::meet)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-with-co-agent-XP-TEMPL )
     )
    ;;;; they met
    ((LF-PARENT ONT::meet)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-PLURAL-TEMPL)
     )
    ;;;; Myrosia 12/31/00 2 streets connect/intersect/meet/cross
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-plural-templ)
     )
    ;;;; street a crosses/meets street b
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
     )
    ((EXAMPLE "it doesn't meet your specifications")
     (LF-PARENT ONT::is-compatible-with)
;     (SEM (F::Time-span F::atomic))
     (TEMPL neutral-neutral-xp-templ)
     (meta-data :origin calo :entry-date 20031206 :change-date nil :comments calo-y1script)
     )
    )
   )
))

