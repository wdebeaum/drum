;;;;
;;;; W::LANDSCAPE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::LANDSCAPE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("landscape%1:15:00"))
     (LF-PARENT ONT::geo-formation)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::landscape
   (SENSES
    ((LF-PARENT ONT::ORIENTATION-VAL)
     (EXAMPLE "lay out the document in landscape orientation")
     (meta-data :origin task-learning :entry-date 20050923 :change-date nil :comments nil)
     )
    )
   )
))

