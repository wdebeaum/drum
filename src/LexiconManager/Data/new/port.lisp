;;;;
;;;; W::PORT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PORT
   (SENSES
    ((LF-PARENT ONT::computer-part) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("port%1:06:01")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n
 :words (
  ((W::PORT W::DE W::SALUT)
  (senses
	   ((LF-PARENT ONT::CHEESE)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

