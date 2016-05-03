;;;;
;;;; W::BECAUSE
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::BECAUSE
   (SENSES
    ((LF-PARENT ONT::REASON)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
     ((LF-PARENT ONT::REASON)
     (TEMPL binary-constraint-s-decl-it-that-templ)
     (example "it's/that's because he is tired")
     (meta-data :origin cardiac :entry-date nil :change-date 20090303 :comments nil)
     )
     ((LF-PARENT ONT::REASON)
     (TEMPL binary-constraint-s-pp-of-templ)
     (Example "I did it because of bad weather")
     (meta-data :origin beetle :entry-date 20081111 :change-date nil :comments nil)
     )
    )
   )
))

