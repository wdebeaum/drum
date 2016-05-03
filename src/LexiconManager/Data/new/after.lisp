;;;;
;;;; W::AFTER
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::AFTER
   (SENSES
    ((LF-PARENT ONT::event-time-rel)
     (example "he left (after 5 pm)")
     (TEMPL binary-constraint-S-implicit-TEMPL)
     )
; 3/2011 conflating sit-val and val roles for ont::event-time-rel
;    ((LF-PARENT ONT::event-time-rel)
;     (TEMPL binary-constraint-SIT-VAL-S-decl-TEMPL)
;     )
    ((LF-PARENT ONT::event-time-rel)
     (example "he left after she arrived")
     (TEMPL binary-constraint-S-decl-TEMPL)
     )
;    ((LF-PARENT ONT::event-time-rel)
;     (TEMPL binary-constraint-SIT-VAL-NP-TEMPL)
;     )
    ((LF-PARENT ONT::event-time-rel)
     (meta-data :origin ralf :entry-date 20040809 :change-date nil :comments nil)
     (example "show me departures after 5 pm")
     (TEMPL binary-constraint-np-TEMPL)
     )
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL BINARY-CONSTRAINT-adj-postpos-TEMPL)
     (meta-data :origin cernl :entry-date 20110114 :change-date nil :comments hpi-acn-3)
     (example "the device placed after that day")
     (preference .98)
     )
    ((LF-PARENT ONT::pos-after-in-trajectory)
     (meta-data :origin beetle :entry-date 20090406 :change-date nil :comments nil)
     (example "after the bridge, turn left" "the bulbs that come after the switch affect it" "people before us in the queue")
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (preference 0.97)
     )
    )
   )
))

