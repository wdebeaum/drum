;;;;
;;;; W::WHICH
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WHICH
   (wordfeats ;(W::sing-lf-only +) 
    (W::case (? cas W::sub W::obj -)))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     
     (SYNTAX (W::wh W::R))
     (TEMPL PRONOUN-WH-TEMPL)
     )
   #|| ((LF-PARENT ONT::REFERENTIAL-SEM)
     (TEMPL PRONOUN-WH-TEMPL)
     (SYNTAX (W::wh W::Q))
     (preference 0.92) ;; MD 2009/03/18 this should only happen if other options are infeasible
     ;; having a higher preference will cause legitimate relative clauses, such as "because the battery is in a closed path which does not contain a bulb", to interpred as wh-questions||#
     )        
    )
   )
 )

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
	 (W::WHICH
	  (wordfeats (W::status ont::*wh-term*) (W::npmod +) (W::negatable +))
	  (SENSES
	   ((LF ONT::WHICH)
	    (non-hierarchy-lf t)(TEMPL wh-qtype-plural-TEMPL)
	    )
	   )
	  )
	 ))

