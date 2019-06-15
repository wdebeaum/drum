;;;;
;;;; w::tie
;;;;

(define-words :pos W::n
 :words (
  (w::tie
  (senses
   ((LF-PARENT ONT::attire)
    (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::tie
   (wordfeats (W::morph (:forms (-vb) :past W::tied :ing w::tying)))
   (SENSES
    ;;;; tie the patient to the stretcher
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    #||((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::bounded) (F::Time-span F::Atomic))
     (example "tie your shoes")
     (TEMPL AGENT-affected-XP-TEMPL)
     )||#
    )
   )
))

