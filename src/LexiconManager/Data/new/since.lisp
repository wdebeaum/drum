;;;;
;;;; W::SINCE
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::SINCE
   (SENSES
    ((LF-PARENT ONT::REASON)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
    ((LF-PARENT ONT::event-time-rel)
     (example "show me arrivals since 5 pm / he left")
     (TEMPL binary-constraint-S-or-NP-TEMPL)
     (meta-data :origin cernl :entry-date 20110105 :change-date nil :comments hpi-acn-4)
     )
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL binary-constraint-S-implicit-TEMPL)
     (example "he has had no problems since")
     (meta-data :origin cernl :entry-date 20110105 :change-date nil :comments hpi-acn-1)
     )
    )
   )
))

