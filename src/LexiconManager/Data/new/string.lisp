;;;;
;;;; W::string
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::string
   (SENSES
    ((LF-PARENT ONT::grouping)
     (TEMPL other-reln-templ)
     (meta-data :origin task-learning :entry-date 20050926 :change-date nil :wn ("string%1:10:00") :comments nil)
     (example "this parameter is a character string")
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::string
   (wordfeats (W::morph (:forms (-vb) :pastpart W::strung)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::joining)
     (example "he strung the beads")
 ; like spray
     )
    )
   )
))

