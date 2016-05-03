;;;;
;;;; W::WHETHER
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::WHETHER
   (SENSES
    ((LF-PARENT ONT::POS-CONDITION)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date 20080623 :comments sentential-conjunction-cleanup)     
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::WHETHER W::OR W::NOT)
   (SENSES
    ((LF-PARENT ONT::POS-CONDITION)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date 20080623 :comments sentential-conjunction-cleanup)     
     )
    )
   )
))

