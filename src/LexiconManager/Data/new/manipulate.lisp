;;;;
;;;; W::manipulate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::manipulate
   (wordfeats (W::morph (:forms (-vb) :nom W::manipulation)))
   (SENSES
    ((EXAMPLE "manipulate the image")
     (LF-PARENT ONT::manipulate)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-THEME-XP-TEMPL)
     ;; changed from ont::interact to ont::manipulate
     (meta-data :origin task-learning :entry-date 20051213 :change-date nil :comments nil)
     )
    )
   )
))

