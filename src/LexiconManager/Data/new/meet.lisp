;;;;
;;;; w::meet
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((w::meet w::up)
    (wordfeats (W::morph (:forms (-vb) :past w::met :nom (w::meet w::up))))
   (senses
    ((meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3 :vn ("herd-47.5.2") :wn ("congregate%2:38:00"))
     (LF-PARENT ONT::meet)
     (example "they met up at the church")
     (TEMPL AGENT-NP-PLURAL-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::meet
   (wordfeats (W::morph (:forms (-vb) :past W::met)))
   (SENSES
    ;;;; he met a friend
    ((LF-PARENT ONT::meet)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AGENT1-XP-NP-TEMPL)
     )
    ;;;;he met with a friend
    ((LF-PARENT ONT::meet)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AGENT1-XP-PP-TEMPL )
     )
    ;;;; they met
    ((LF-PARENT ONT::meet)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-NP-PLURAL-TEMPL)
     )
    ;;;; Myrosia 12/31/00 2 streets connect/intersect/meet/cross
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NP-PLURAL-TEMPL)
     )
    ;;;; street a crosses/meets street b
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    ((EXAMPLE "it doesn't meet your specifications")
     (LF-PARENT ONT::is-compatible-with)
     ;(SEM (F::Time-span F::atomic))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (meta-data :origin calo :entry-date 20031206 :change-date nil :comments calo-y1script)
     )
    )
   )
))

