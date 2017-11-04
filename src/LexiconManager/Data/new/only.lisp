;;;;
;;;; W::ONLY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::ONLY
   (wordfeats (W::comparative +))
   (SENSES
    ;;;;; use this to allow it to appear in the + comparative-adj + cardinal rule
    ;;;;; the only two trucks, the best two trucks
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("only%5:00:00:single:05"))
     (LF-PARENT ONT::CARDINALITY-VAL)
     (SEM (F::GRADABILITY F::-))
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::ONLY
   (SENSES
    ((LF-PARENT ONT::RESTRICTION)
     (LF-FORM W::only)
     (TEMPL PRED-S-VP-TEMPL)
     )
    ((LF-PARENT ONT::MODIFIER)
     (LF-FORM W::ONLY)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::ONLY W::IF)
   (SENSES
    ((LF-PARENT ONT::POS-CONDITION)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date 20080623 :comments sentential-conjunction-cleanup)     
     )
    ((LF-PARENT ONT::POS-CONDITION)
     (TEMPL binary-constraint-gerund-templ)
     (example "do it only if walking")
     (meta-data :origin beetle2 :entry-date 20070609 :change-date 20080623 :comments sentential-conjunction-cleanup)     
     )    
    )
   )
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  ;; Only is definitely a quantifier in "in circuit 1 only the bulb is in a closed path"
  (W::ONLY
   (wordfeats (W::status ont::quantifier) (W::npmod +) (W::negatable +) (w::mass ?mass) (W::AGR (? agr W::3S W::3P)))
   (SENSES
    ((LF ONT::ONLY) 
     (non-hierarchy-lf t)(TEMPL quan-no-bare-templ)
     )
    )
   )   
))

