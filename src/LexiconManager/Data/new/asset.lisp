;;;;
;;;; W::asset
;;;;

(define-words :pos W::n 
 :words (
	 (W::asset
	  (SENSES
	   ((LF-PARENT ONT::ATTRIBUTE)
	    (TEMPL COUNT-PRED-TEMPL)
	    (meta-data :origin sense :entry-date 20070724 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

(define-words :pos W::n 
 :words (
	 (W::assets  ; plural only
	  (wordfeats (W::morph (:forms (-none))))
	  (SENSES
	   (
	    (LF-PARENT ONT::assets)
;	    (LF-PARENT ONT::FUNCTIONAL-PHYS-OBJECT)
	    (TEMPL COUNT-PRED-3p-TEMPL)
	    (meta-data :origin sense :entry-date 20070724 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

