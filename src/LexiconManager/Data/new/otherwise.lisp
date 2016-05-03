;;;;
;;;; W::OTHERWISE
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::OTHERWISE
   (SENSES
   ((LF-PARENT ONT::choice-option )
     (TEMPL binary-constraint-s-decl-templ)
     ;; We can't say "otherwise it will break, you must do it"
 ;    (SYNTAX (W::ATYPE W::POST))
     (example "it will break otherwise" "otherwise it will break")
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )     
    ((LF-PARENT ONT::choice-option)
     (TEMPL pred-s-vp-templ)
     (example "it will otherwise change")
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
    )
   )
))

