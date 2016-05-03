;;;;
;;;; W::archive
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::archive
   (SENSES
    ((LF-PARENT ONT::database)
     (EXAMPLE "open the archive")
     (meta-data :origin task-learning :entry-date 20050816 :change-date nil :wn ("archive%1:06:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::archive
     (wordfeats (W::morph (:forms (-vb) :nom w::archive)))
   (SENSES
    ((EXAMPLE "archive a calendar")
     (meta-data :origin task-learning :entry-date 20050816 :change-date nil :comments nil)
     (LF-PARENT ONT::archive)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

