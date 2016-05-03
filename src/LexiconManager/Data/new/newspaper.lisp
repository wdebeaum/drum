;;;;
;;;; W::NEWSPAPER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::NEWSPAPER
   (SENSES
    ((LF-PARENT ONT::publication) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("newspaper%1:10:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::newspaper
   (SENSES
    ((meta-data :origin calo :entry-date 20050325 :change-date nil :wn ("newspaper%1:10:00") :comments caloy2)
     (LF-PARENT ONT::info-medium)
     (example "i read it in the newspaper")
     )
    )
   )
))

