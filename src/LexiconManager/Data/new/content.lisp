;;;;
;;;; W::content
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::content
   (SENSES
    ((LF-PARENT ONT::content)
     (EXAMPLE "Mail displays the contents of messages")
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :wn ("content%1:14:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::content
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("content%2:37:00"))
     (LF-PARENT ONT::evoke-satisfaction)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     )
    )
   )
))

