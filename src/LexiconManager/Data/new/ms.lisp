;;;;
;;;; W::ms
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::ms
   (wordfeats (W::morph (:forms (-s-3p) :plur W::ms)))
   (SENSES
    ((LF-PARENT ONT::time-unit)
     (LF-FORM W::millisecond)
     (TEMPL UNIT-TEMPL)
     (meta-data :origin step :entry-date 20080711 :change-date nil :comments nil :wn ("s%1:28:00"))
     )
    )
   )
))

(define-words :pos W::name
 :words (
  (w::ms
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

(define-words :pos W::name
 :words (
  ((w::ms W::punc-period)
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

