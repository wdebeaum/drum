;;;;
;;;; W::MIKE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MIKE
   (SENSES
    ((LF-PARENT ONT::device) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("mike%1:06:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::value :boost-word t
 :words (
  (w::mike
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

