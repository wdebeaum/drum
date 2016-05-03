;;;;
;;;; W::THOUGH
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
;;;;;;;;;;;;;;;;;;;; Start added list of sentenial connectives
  (W::THOUGH
   (SENSES
    ((LF-PARENT ONT::Qualification )
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date nil :comments sentential-conjunction-cleanup)
     )
    ((LF-PARENT ONT::QUALIFICATION)
     (TEMPL DISC-POST-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
    )
   )
))

