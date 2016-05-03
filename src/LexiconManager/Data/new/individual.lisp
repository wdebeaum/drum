;;;;
;;;; W::INDIVIDUAL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::INDIVIDUAL
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("individual%1:03:00"))
     (LF-PARENT ONT::PERSON)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::individual
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((EXAMPLE "a user can create individual calendars")
     (LF-PARENT ONT::singularity-VAL) 
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :wn ("individual%3:00:00") :comments nil)
     )
    )
   )
))

