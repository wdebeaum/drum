;;;;
;;;; W::MAYBE
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
;;;;;;;;;;;;;;;;;;;; End added list of sentential connectives
  (W::MAYBE
   (SENSES
    ((LF-PARENT ONT::QUALIFICATION)
     (LF-FORM W::maybe)
     (TEMPL PRED-VP-TEMPL)
     )
    ((LF-PARENT ONT::QUALIFICATION)
     (meta-data :origin calo :entry-date 20040406 :change-date nil :comments y1v5)
     (example "a few thousand maybe")
     (LF-FORM W::maybe)
     (TEMPL DISC-TEMPL)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::MAYBE
   (SENSES
    ((LF (ONT::UNSURE-POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))

