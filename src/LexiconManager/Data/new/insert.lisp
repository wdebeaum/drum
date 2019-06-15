;;;;
;;;; W::insert
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-RESULT-ADVBL-OBJCONTROL-TEMPL
 :words (
  (W::insert
   (wordfeats (W::morph (:forms (-vb) :nom w::insertion)))
   (SENSES
    (;(LF-PARENT ONT::put)
     (LF-PARENT ONT::cause-in)
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :comments nil :vn ("put-9.1"))
     (example "You can insert additional characters")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

