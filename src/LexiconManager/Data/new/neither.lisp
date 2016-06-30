;;;;
;;;; W::neither
;;;;

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  (W::neither
   (SENSES
    ((LF ONT::NEITHER)
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
     (non-hierarchy-lf t) (TEMPL SUBCAT-DOUBLE-CONJ-TEMPL)
     (SYNTAX (w::subcat2 w::nor) (w::operator ont::none-of) 
	     (w::status w::quantifier) (w::agr ?agr)
	     (W::disj +) (w::conj -) (W::seq +) (w::neg +)
	     )
     )
    )
   )
))

