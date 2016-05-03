;;;;
;;;; W::VALUE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::VALUE
   (SENSES
    ((LF-PARENT ONT::value-cost) (TEMPL other-reln-templ)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("value%1:07:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::value
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090522 :comments nil :vn ("price-54.4") :wn ("value%2:31:00" "value%2:31:02" "value%2:31:03"))
     (LF-PARENT ONT::scrutiny)
     (templ agent-neutral-xp-templ)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("admire-31.2") :wn ("value%2:31:01" "value%2:40:00"))
     (LF-PARENT ONT::appreciate)
     (TEMPL experiencer-neutral-xp-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     )
    )
   )
))

