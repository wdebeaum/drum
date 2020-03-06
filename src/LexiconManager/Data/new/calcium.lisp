;;;;
;;;; W::CALCIUM
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CALCIUM
   (SENSES
    ((LF-PARENT ONT::substance) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("calcium%1:27:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n
 :words (
  ((W::CALCIUM W::PROPIONATE)
  (senses
	   ((LF-PARENT ONT::INGREDIENTS)
	    (syntax (W::morph (:forms (-none))))
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  (W::CALCIUM
  (senses
	   ((LF-PARENT ONT::MINERALS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))
#|
(define-words :pos W::n
 :words (
  ((W::CALCIUM W::PHOSPHORUS)
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
  ((W::CALCIUM W::SULFATE)
  (senses
	   ((LF-PARENT ONT::MINERALS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))
|#

