;;;;
;;;; W::redo
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::redo
   (wordfeats (W::morph (:forms (-vb) :past W::redid :pastpart W::redone :3s W::redoes)))
   (SENSES
    ((LF-PARENT ONT::EXECUTE)
     (meta-data :origin calo :entry-date 20040622 :change-date nil :comments y2)
     (example "redo it")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     )
    )
   )
))

