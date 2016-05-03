;;;;
;;;; w::both
;;;;

(define-words 
    :pos W::adv :templ pred-s-post-templ
 :tags (:base500)
 :words (
	    (w::both
	     (senses
	      ((LF-PARENT ONT::Modifier) ;; NO GOOD PLACE YET -- WILL HAVE TO SEE
	       (TEMPL PRED-VP-PRE-TEMPL)
	       (example "they both will be off")
	       (preference 0.91)
	       (meta-data :origin beetle2 :entry-date 20080513 :change-date nil :comments (pilot2) :wn nil)
	       )
	      ))
))

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  (W::BOTH
   (wordfeats (W::conj +) (W::seq +))
   (SENSES
    ((LF W::BOTH)
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
     (non-hierarchy-lf t) (TEMPL SUBCAT-DOUBLE-CONJ-TEMPL)
     (SYNTAX (w::subcat2 w::and) (w::operator w::both) 
	     (w::status w::definite-plural) (w::agr w::3p)
	     (W::disj -) (w::conj +) (W::seq +)      
	     )
     )
    ))
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::BOTH
   (wordfeats (W::QUANT 2) (W::status W::definite-plural) (W::NPmod +))
   (SENSES
    ((LF W::BOTH)
     (non-hierarchy-lf t)
     (example "both us and european voltage")
     (TEMPL quan-3p-templ)
     )
    )
   )
))

