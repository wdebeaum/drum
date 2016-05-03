;;;;
;;;; W::UNLESS
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::UNLESS
   (SENSES
    ((LF-PARENT ONT::NEG-CONDITION)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date 20080623 :comments sentential-conjunction-cleanup)     
     )
    ((LF-PARENT ONT::NEG-CONDITION)
     (TEMPL binary-constraint-gerund-templ)
     (example "be careful unless walking")
     (meta-data :origin beetle2 :entry-date 20070609 :change-date 20080623 :comments sentential-conjunction-cleanup)     
     )    
    )
   )
))

