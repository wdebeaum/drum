;;;;
;;;; W::AIRPORT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::AIRPORT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20070622 :comments nil :wn ("airport%1:06:00"))
     (LF-PARENT ONT::airport)
;     (preference 1.0)    ;;  quick fix for PLOW demo    JFA 6/7/06
     )
   ((meta-data :origin calo :entry-date 20031229 :change-date nil :comments html-purchasing-corpus)
    (LF-PARENT ONT::computer-card)
    (preference .9)
    )
    )
   )
))

