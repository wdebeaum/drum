;;;;
;;;; W::FEE
;;;;

#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::FEE
   (SENSES
    ((LF-PARENT ONT::value-cost) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("fee%1:21:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))
|#

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::FEE
   (SENSES
    ((LF-PARENT ONT::value-COST)
     (TEMPL OTHER-RELN-TEMPL)
     (meta-data :origin calo :entry-date 20031206 :change-date nil :wn ("fee%1:21:00") :comments calo-y1script )
     )
    ((LF-PARENT ONT::value-COST)
     (TEMPL reln-subcat-of-units-TEMPL)
     (meta-data :origin calo :entry-date 20031206 :change-date nil :wn ("fee%1:21:00") :comments calo-y1script )
     )
    )
   )
))

