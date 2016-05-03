;;;;
;;;; W::PHOTO
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PHOTO
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("photo%1:06:00"))
     (LF-PARENT ONT::IMAGE)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PHOTO
   (SENSES
    ((LF-PARENT ONT::image) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("photo%1:06:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

