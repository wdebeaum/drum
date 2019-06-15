;;;;
;;;; W::indent
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::indent
   (SENSES
    ((lf-parent ont::arrange-text)
     (example "indent the text")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     (meta-data :origin task-learning :entry-date 20050815 :change-date 20090504 :comments nil)
     )
    )
   )
))

