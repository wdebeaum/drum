;;;;
;;;; W::introduce
;;;;

(define-words :pos W::v 
 :words (
  (W::introduce
   (wordfeats (W::morph (:forms (-vb) :nom W::introduction)))
   (SENSES
    (
     (LF-PARENT ONT::create) 
     (meta-data :origin task-learning :entry-date 20050831 :change-date nil :comments nil)
     (example "others introduce problems in the library")
     (TEMPL AGENT-AFFECTEDR-XP-TEMPL)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

