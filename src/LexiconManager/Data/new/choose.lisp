;;;;
;;;; W::choose
;;;;

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
  (W::choose
   (wordfeats (W::morph (:forms (-vb) :past W::chose :pastpart W::chosen)))
   (SENSES
   ((LF-PARENT ONT::SELECT)
    (example "choose this one")
    (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
    )
   ((meta-data :origin plow :entry-date 20060531 :change-date nil :comments nil)
    (LF-PARENT ONT::SELECT)
    (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
    (EXAMPLE "choose red")
    (templ agent-theme-pred-templ)
    )
   ((LF-PARENT ONT::SELECT)
     (TEMPL agent-action-SUBJCONTROL-TEMPL)
    (meta-data :origin jr :entry-date 20120621 :change-date nil :comments gloss-variant)
     (EXAMPLE "to choose not to consume")
     )
   )
  )
))

