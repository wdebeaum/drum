;;;;
;;;; W::terminate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::terminate
    (wordfeats (W::morph (:forms (-vb) :nom w::termination)))
   (SENSES
    ((EXAMPLE "He terminated the meeting/document")
     (LF-PARENT ONT::STOP)
     (templ agent-affected-xp-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050831 :change-date nil :comments nil :vn ("stop-55.4"))
     )
    ((LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-TEMPL)
     (EXAMPLE "The program terminated")
     (meta-data :origin task-learning :entry-date 20050831 :change-date nil :comments nil)
     )
    )
   )
))

