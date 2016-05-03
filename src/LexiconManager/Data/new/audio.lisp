;;;;
;;;; W::AUDIO
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::AUDIO
   (SENSES
    ((LF-PARENT ONT::audio) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
		:COMMENTS HTML-PURCHASING-CORPUS))
    ;; MD FIXME potential duplicate
    ((LF-PARENT ONT::AUDIO)
     (meta-data :origin calo :entry-date 20050215 :change-date nil :wn ("audio%1:10:00") :comments caloy2)
     )
    )
   )
))

