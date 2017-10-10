;;;;
;;;; W::PERIPHERAL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PERIPHERAL
   (SENSES
    ((LF-PARENT ONT::device) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("peripheral%1:06:00")
      :COMMENTS HTML-PURCHASING-CORPUS))
))
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (w::peripheral
    (SENSES
     ((LF-PARENT ONT::peripheral-val)
      (meta-data :origin domain-reorganization :entry-date 20170821 :change-date nil)
      )
     )
    )
))
