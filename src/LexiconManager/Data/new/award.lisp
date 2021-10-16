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

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::award
   (SENSES
    #|
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("future_having-13.3") :wn ("award%2:40:00" "award%2:40:01" "award%2:41:00"))
     (LF-PARENT ONT::giving)
     (TEMPL AGENT-AFFECTED-TEMPL) ; like grant,offer
     )
    |#
   ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("future_having-13.3") :wn ("award%2:40:00" "award%2:40:01" "award%2:41:00"))
     (LF-PARENT ONT::giving)
     ;(TEMPL agent-affected-goal-optional-templ) ; like grant,offer
     (TEMPL AGENT-AFFECTED-AFFECTEDR-XP-optional-TEMPL)
     )
    )
   )
))

