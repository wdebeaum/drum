;;;;
;;;; W::COLON
;;;;

(define-words :pos W::n
 :words (
;; internal
  (W::COLON
  (senses((LF-PARENT ONT::internal-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::colon
   (SENSES
    ((LF-PARENT ONT::punctuation)
     (EXAMPLE "separate the lines with colons")
     (meta-data :origin task-learning :entry-date 20050923 :change-date nil :wn ("colon%1:10:00") :comments nil)
     )
    )
   )
))

