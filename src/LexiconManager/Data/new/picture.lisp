;;;;
;;;; W::PICTURE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::PICTURE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::IMAGE)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::picture
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("characterize-29.2"))
     (LF-PARENT ONT::encodes-message)
     (TEMPL NEUTRAL-NEUTRAL1-FORMAL-2-XP-3-XP2-PP-TEMPL) ; like interpret,classify
     )
    )
   )
))

