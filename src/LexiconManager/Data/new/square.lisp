;;;;
;;;; w::square
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
((w::square W::Feet)
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin sense :entry-date 20090915 :change-date nil :comments nil :wn ("foot%1:23:00"))
     (LF-PARENT ONT::area-unit)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     (SYNTAX (W::AGR W::3P))
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
((w::square W::FOOT)
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin sense :entry-date 20090915 :change-date nil :comments nil :wn ("foot%1:23:00"))
     (LF-PARENT ONT::area-unit)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     (SYNTAX (W::AGR W::3S))
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SQUARE
   (SENSES
    ((meta-data :origin fruit-carts :entry-date 20041103 :change-date nil :wn ("square%1:25:00") :comments nil)
     (example "take the square with the heart on the side")
     (LF-PARENT ONT::SHAPE-OBJECT)
     )
    )
   )
))

(define-words :pos w::N 
 :words (
;; Mathematical functions
  ((w::square w::root)
  (senses((LF-parent ONT::Number-measure-domain) 
	    (templ other-reln-templ)
	    (meta-data :origin lam :entry-date 20050420 :change-date nil :comments lam-initial)
	    ))
  ;; Added by Myrosia during LAM extension
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::SQUARE
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("square%3:00:00"))
     (LF-PARENT ONT::SHAPE-val)
     )
    )
   )
))

