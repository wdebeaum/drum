;;;;
;;;; W::agenda
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::agenda
   (SENSES
    ((EXAMPLE "what is the agenda for the meeting")
     (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :wn ("agenda%1:09:00" "agenda%1:10:00") :comments nil)
     (LF-PARENT ONT::commitment)
     )
    ((LF-PARENT ONT::info-medium) ;; like calendar
     (EXAMPLE "let me write it in my agenda")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Office)
     )
    )
   )
))

