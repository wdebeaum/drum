;;;;
;;;; W::AWARD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::AWARD
   (SENSES
    ((LF-PARENT ONT::prize) (TEMPL other-reln-templ)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::award
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("future_having-13.3") :wn ("award%2:40:00" "award%2:40:01" "award%2:41:00"))
     (LF-PARENT ONT::giving)
     (TEMPL agent-affected-recipient-alternation-templ) ; like grant,offer
     )
   ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("future_having-13.3") :wn ("award%2:40:00" "award%2:40:01" "award%2:41:00"))
     (LF-PARENT ONT::giving)
     (TEMPL agent-affected-goal-optional-templ) ; like grant,offer
     )
    )
   )
))

