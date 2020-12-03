;;;;
;;;; W::SCREEN
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SCREEN
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::DISPLAY)
     (TEMPL BARE-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::SCREEN W::SAVER)   
   (SENSES
    ((LF-PARENT ONT::computer-system) ;computer-software) 
     (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("screen_saver%1:06:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

