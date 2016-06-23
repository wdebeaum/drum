;;;;
;;;; W::BEFORE
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::BEFORE
   (SENSES
; 3/2011 conflating sit-val and val roles for ont::event-time-rel
;    ((LF-PARENT ONT::event-time-rel)
;     (TEMPL binary-constraint-SIT-VAL-S-decl-TEMPL)
;     )
;    ((LF-PARENT ONT::event-time-rel)
;     (TEMPL binary-constraint-SIT-VAL-NP-TEMPL)
;     )
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL binary-constraint-S-decl-TEMPL)
     (example "he arrived before she left")
     )
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL binary-constraint-S-implicit-TEMPL)
     (example "he arrived before (5 pm)")
     )
    ((LF-PARENT ONT::event-time-rel)
     (meta-data :origin ralf :entry-date 20040809 :change-date nil :comments nil)
     (example "show me departures before 5 pm")
     (TEMPL binary-constraint-np-TEMPL)
     )
    ;; This one should now be handled by the adj-verb rules
   #|| ((LF-PARENT ONT::event-time-rel)
     (TEMPL BINARY-CONSTRAINT-adj-postpos-TEMPL)
     (meta-data :origin cernl :entry-date 20110114 :change-date nil :comments hpi-acn-3)
     (example "the device placed before that day")
     (preference .98)
     )||#   
    ((LF-PARENT ONT::pos-before-in-trajectory)
     (meta-data :origin beetle :entry-date 20090406 :change-date nil :comments nil)
     (example "just before the bridge, turn left" "the bulbs that come before the switch affect it")
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (preference 0.96)
     )
    )
   )
))

