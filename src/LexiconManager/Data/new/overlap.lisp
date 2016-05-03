;;;;
;;;; W::overlap
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::overlap
   (SENSES
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
     (example "the banana overlaps the avocado")
     (meta-data :origin fruitcarts :entry-date 20050401 :change-date nil :comments nil)
     )
    )
   )
))

