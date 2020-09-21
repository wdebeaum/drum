;;;;
;;;; W::sponsor
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
   (W::sponsor
   (SENSES
    ((LF-PARENT ONT::benefactor) ;professional)
     (EXAMPLE "confirm the sponsor name")
     (meta-data :origin plot :entry-date 20080812 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::sponsor
   (SENSES
    ((LF-PARENT ONT::managing-resources)
     (example "sponsor an event")
     (meta-data :origin plow :entry-date 20060712 :change-date nil :comments nil)
     )
    )
   )
))

