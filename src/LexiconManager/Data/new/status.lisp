;;;;
;;;; W::status
;;;;

#|
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::status W::post)
   (SENSES
    ((LF-PARENT ONT::discrete-property-VAL)
     (meta-data :origin cernl :entry-date 20101029 :change-date nil :wn ("former%3:00:00") :comments nil)
     (EXAMPLE "history of diverticulitis, status post cholecystectomy")
     )
    )
   )
))
|#

(define-words :pos W::ADV
 :words (
  ((W::status W::post)
   (SENSES
    ((LF-PARENT ONT::event-time-rel) ; should be with 'after' and event-time-rel needs subdivision
     (meta-data :origin cernl :entry-date 20100725 :change-date nil :comments nil)
     (example "status post surgery")
     (TEMPL binary-constraint-NP-TEMPL)
     ))
   )
))

