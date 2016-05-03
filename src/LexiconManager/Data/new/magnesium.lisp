;;;;
;;;; W::MAGNESIUM
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MAGNESIUM
   (SENSES
    ((LF-PARENT ONT::substance) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("magnesium%1:27:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n
 :words (
  (w::magnesium
  (senses
	   ((LF-PARENT ONT::MINERALS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::MAGNESIUM W::CHLORIDE)
  (senses
	   ((LF-PARENT ONT::MINERALS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

