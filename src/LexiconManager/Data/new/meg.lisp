;;;;
;;;; W::MEG
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MEG
   (SENSES
    ((LF-PARENT ONT::memory-unit)
     (LF-FORM W::megabyte)
     (TEMPL SUBSTANCE-UNIT-lf-of-3s-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::MEG ;; alternate plural
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20070813 :comments nil :wn ("mb%1:23:00"))
     (LF-PARENT ONT::memory-unit)
     (LF-FORM W::megabyte)
     (example " 5 meg of ram")
     (TEMPL SUBSTANCE-UNIT-lf-of-3s-plur-TEMPL)
     )
    )
   )
))

