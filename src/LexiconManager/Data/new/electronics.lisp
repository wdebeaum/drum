;;;;
;;;; W::ELECTRONICS
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ELECTRONICS
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::discipline) (TEMPL count-PRED-3p-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("electronics%1:09:00")
      :COMMENTS HTML-PURCHASING-CORPUS))
    ((LF-PARENT ONT::device) (TEMPL count-PRED-3p-TEMPL)
     (META-DATA :ORIGIN CALO-ontology :ENTRY-DATE 20051213 :CHANGE-DATE NIL
      :COMMENTS Office))
    )
   )
))

