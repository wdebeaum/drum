;;;;
;;;; W::party
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::party
   (SENSES
    ((LF-PARENT ONT::person)
     (EXAMPLE "an authorized party")
     (meta-data :origin task-learning :entry-date 20050822 :change-date nil :wn ("party%1:18:00") :comments nil)
     (preference .97) ;; prefer event sense
     )
    ((LF-PARENT ONT::gathering-event)
     (EXAMPLE "go to the party")
     (meta-data :origin calo-ontology :entry-date 20060524 :change-date nil :wn ("party%1:14:00") :comments nil)
     )
    )
   )
))

