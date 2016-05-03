;;;;
;;;; W::bullet
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::bullet
   (SENSES
    ((LF-PARENT ONT::punctuation)
     (EXAMPLE "drag the bullet to the right")
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::bullet
   (SENSES
    ((lf-parent ont::arrange-text)
     (example "bullet the text")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ)
     (meta-data :origin task-learning :entry-date 20050815 :change-date 20090504 :comments nil)
     )
    )
   )
))

