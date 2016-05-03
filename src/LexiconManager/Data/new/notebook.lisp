;;;;
;;;; W::NOTEBOOK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::NOTEBOOK
   (SENSES
    ((LF-PARENT ONT::computer-type) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("notebook%1:06:00")
      :COMMENTS HTML-PURCHASING-CORPUS)
     )
    ((LF-PARENT ONT::info-medium) ;; like folder
     (EXAMPLE "write it in your notebook")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :wn ("notebook%1:10:00") :comments Office)
     )
    ))
))

