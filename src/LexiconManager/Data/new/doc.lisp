;;;;
;;;; W::DOC
;;;;

(define-words :pos W::n
 :words (
  (W::DOC
  (senses
   ((LF-PARENT ONT::health-professional)
    (TEMPL COUNT-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::doc
   (SENSES
    ((LF-PARENT ONT::file-format)
     (EXAMPLE "export the document to doc")
     (meta-data :origin task-learning :entry-date 20050824 :change-date nil :comments nil)
     )
    )
   )
))

