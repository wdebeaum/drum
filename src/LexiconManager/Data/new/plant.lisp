;;;;
;;;; W::PLANT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::PLANT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("plant%1:06:01"))
     (LF-PARENT ONT::production-facility)
     )
    ((LF-PARENT ONT::plant)
     (example "could you water my plants while I'm away")
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("plant%1:03:00") :comments Office)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::plant
   (SENSES
    ((LF-PARENT ONT::sow-seed)
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil)
     (example "plant the cactus")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

