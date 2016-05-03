;;;;
;;;; w::but
;;;;

(define-words :pos w::adv
  :words (
	  ;; Parentheticals
	  ((w::but w::not)
	   (senses
	    ((TEMPL BINARY-CONSTRAINT-NP-TEMPL)
	     (LF-PARENT ONT::PARENTHETICAL)
	     (meta-data :origin beetle :entry-date 20090116 :change-date nil :comments nil)
	     (preference .98) ;; prefer the ADV operator when possible
	     )
	    ((TEMPL  NEG-ADJ-ADV-OPERATOR-TEMPL)
	     (LF-PARENT ONT::PARENTHETICAL)
	     (LF-FORM W::BUT-NOT)
	     )
	    )
	   )
	  ))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::BUT
   (SENSES
    ((LF-PARENT ONT::CONJUNCT)
     )
    )
   )
))

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  (W::BUT
   (wordfeats (W::conj +))
   (SENSES
    ((LF W::BUT)
     (non-hierarchy-lf t)(TEMPL SUBCAT-ANY-TEMPL)
     )
    )
   )
  
  ))

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  ((W::BUT w::not)
   (wordfeats (W::conj +))
   (SENSES
    ((LF W::BUT-NOT)
     (non-hierarchy-lf t)
     (TEMPL SUBCAT-ANY-TEMPL)
     )
    )
   )
  
  ))

