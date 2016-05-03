;;;;
;;;; W::RENAME
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::RENAME
   (SENSES
    ((LF-PARENT ONT::naming)
     (meta-data :origin task-learning :entry-date 20050817 :change-date 20090501 :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "rename a mailbox")
     (TEMPL agent-neutral-name-optional-templ)
     )
     ;; need this definition to allow v-passive-by rule to apply; comp arg must be -
    ((LF-PARENT ONT::naming)
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090501 :comments caloy3)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "this was renamed by him")
     (TEMPL agent-neutral-xp-templ)
     )
    )
   )
))

