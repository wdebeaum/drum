;;;;
;;;; W::sponsor
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
   (W::sponsor
   (SENSES
    ((LF-PARENT ONT::professional)
     (EXAMPLE "confirm the sponsor name")
     (meta-data :origin plot :entry-date 20080812 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::sponsor
   (SENSES
    ((LF-PARENT ONT::host)
     (example "sponsor an event")
     (meta-data :origin plow :entry-date 20060712 :change-date nil :comments nil)
     )
    )
   )
))

