;;;;
;;;; W::AGREEMENT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::AGREEMENT
   (SENSES
    ((LF-PARENT ONT::accept-agree) (TEMPL COUNT-PRED-TEMPL)
     (example "they reached an agreement")
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE 20090511 :wn ("agreement%1:26:01")
      :COMMENTS HTML-PURCHASING-CORPUS))
    ((LF-PARENT ONT::official-document)
     (EXAMPLE "read the licensing agreement before you install the software")
     (meta-data :origin task-learning :entry-date 20050901 :change-date nil :wn ("agreement%1:10:01") :comments nil)
     )
    )
   )
))

