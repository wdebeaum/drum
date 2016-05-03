;;;;
;;;; W::FACTOR
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::FACTOR
   (SENSES
    ((LF-PARENT ONT::part)
     (example "there are several factors that contribute to his decision")
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("factor%1:09:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::factor
   (SENSES
    ((LF-PARENT ONT::Mathematical-term)
     (TEMPL count-pred-templ)
     (meta-data :origin lam :entry-date 20050707 :change-date nil :wn ("factor%1:23:01") :comments nil)
     )
  ))
))

