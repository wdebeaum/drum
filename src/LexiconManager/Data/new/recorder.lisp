;;;;
;;;; W::RECORDER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::RECORDER
   (SENSES
    ((LF-PARENT ONT::recording-device) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("recorder%1:06:01")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n
 :words (
  (w::recorder
  (senses
   ((LF-PARENT ONT::musical-instrument)
    (TEMPL count-pred-TEMPL)
    (meta-data :origin cardiac :entry-date 20080509 :change-date nil :comments LM-vocab)
    )
   )
)
))

