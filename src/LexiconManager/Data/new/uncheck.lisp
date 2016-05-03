;;;;
;;;; w::uncheck
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (w::uncheck
   (SENSES
    ((EXAMPLE "Check or uncheck the selected calendar in the Calendars list")
     (LF-PARENT ONT::select)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     )
    )
   )
))

