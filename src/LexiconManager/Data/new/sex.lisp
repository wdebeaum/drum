;;;;
;;;; W::sex
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::sex
   (SENSES
    ((LF-PARENT ONT::gender-scale)
     (TEMPL OTHER-RELN-TEMPL)
     (EXAMPLE "the sex of the dog is male")
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :wn ("sex%1:07:00") :comments nil)
     )
    ((LF-PARENT ONT::event-defined-by-activity) ;action)
     (EXAMPLE "adjust the content levels in each of four areas: violence, sex, nudity, and language")
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :wn ("sex%1:04:00") :comments nil)
     )
    )
   )
))

