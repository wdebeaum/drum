;;;;
;;;; W::ALARM
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::ALARM
     (SENSES
      ((LF-PARENT ONT::device) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::alarm
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("alarm%2:37:00"))
     (LF-PARENT ONT::evoke-worry)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     )
    )
   )
))

