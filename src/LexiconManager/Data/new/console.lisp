;;;;
;;;; W::CONSOLE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CONSOLE
   (SENSES
    ((LF-PARENT ONT::computer-part) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("console%1:06:02")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::console
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date nil :comments nil :vn ("amuse-31.1") :wn ("console%2:37:00"))
     (LF-PARENT ONT::EVOKE-CALM)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     )
    )
   )
))

