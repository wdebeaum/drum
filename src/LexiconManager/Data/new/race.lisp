;;;;
;;;; W::RACE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::RACE
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060406 :change-date nil :wn ("race%1:11:01") :comments nil)
     (LF-PARENT ONT::gathering-event)
     )
    )
   )
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
   (W::race
     (wordfeats (W::morph (:forms (-vb) :nom W::race)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("race%2:38:00" "race%2:38:10"))
     (LF-PARENT ONT::move-rapidly)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

