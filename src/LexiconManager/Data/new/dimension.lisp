;;;;
;;;; W::DIMENSION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DIMENSION
   (SENSES
    ((LF-PARENT ont::linear-extent-scale) (TEMPL count-pred-templ)
     (EXAMPLE "The dimensions of the pond were 14ft by 18ft")
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS :wn ("dimension%1:07:00")))
    ((LF-PARENT ont::dimensional-scale) (TEMPL count-pred-templ)
     (EXAMPLE "A building of vast dimensions/proportions")
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS :wn ("dimension%1:07:01")))
))
))

