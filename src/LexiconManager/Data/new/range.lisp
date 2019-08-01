;;;;
;;;; W::RANGE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::RANGE
   (SENSES
    ((meta-data :origin calo :entry-date 20031229 :change-date 20070523 :comments html-purchasing-corpus)
     (LF-PARENT ONT::range)
;     (LF-PARENT ONT::level)
     (example "this is the date range")
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v
 :words (
  (W::RANGE
   (SENSES
    ((LF-PARENT ONT::location-as-motion)
     (example "The grass ranges over the hills")
     (TEMPL NEUTRAL-LOCATION-XP-TEMPL)
     )
    )
   )
))
