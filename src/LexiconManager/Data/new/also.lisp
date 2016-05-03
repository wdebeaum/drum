;;;;
;;;; W::ALSO
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::ALSO
   (SENSES
    ((LF-PARENT ONT::CONJUNCT)
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
    ((LF-PARENT ONT::ADDITIVE)
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     (LF-FORM W::ALSO)
     (TEMPL PRED-VP-TEMPL)
     )
    )
   )
))

