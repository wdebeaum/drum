;;;;
;;;; W::DURING
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::DURING
   (SENSES
    #|
    ((LF-PARENT ONT::situated-in)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (example "during the meeting")
     )
    |#
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     (meta-data :origin step :entry-date 20080530 :change-date nil :comments nil)
     (example "during the last week" "during the meeting")
     )
    )
   )
))

