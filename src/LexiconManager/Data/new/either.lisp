;;;;
;;;; W::either
;;;;

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  (W::either
   (wordfeats (W::disj +) (W::seq +))
   (SENSES
    ((LF W::EITHER)
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
     (non-hierarchy-lf t) (TEMPL SUBCAT-DOUBLE-CONJ-TEMPL)
     (SYNTAX (w::subcat2 w::or) (w::operator w::one-of) 
	     (w::status w::indefinite) (w::agr ?agr)
	     (W::disj +) (w::conj -) (W::seq +)      
	     )
     )
    ))
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::EITHER
   (wordfeats (W::status W::quantifier) (W::MASS ?m) (W::AGR W::3s))
   (SENSES
    ((LF W::EITHER)
     (non-hierarchy-lf t)
     (TEMPL quan-sing-count-TEMPL)))
   )
))

