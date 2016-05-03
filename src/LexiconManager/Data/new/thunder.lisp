;;;;
;;;; W::thunder
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::thunder
   (SENSES
    ((LF-PARENT ONT::ATMOSPHERIC-PHENOMENON)
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :wn ("thunder%1:11:00" "thunder%1:11:01") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::thunder
   (wordfeats (W::morph (:forms (-vb) :past W::thundered :ing W::thundering)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("thunder%2:43:00"))
     (LF-PARENT ont::atmospheric-event)
     (TEMPL expletive-templ) ; like rain
     )
    )
   )
))

