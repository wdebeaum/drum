;;;;
;;;; W::IF
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::IF
   (SENSES
    ((LF-PARENT ONT::POS-CONDITION)
     (Example "if I see it then I will believe it")
     (TEMPL binary-constraint-s-decl-middle-word-subcat-templ (xp2 (% w::word (w::lex w::then))))
     (meta-data :origin beetle2 :entry-date 20070609 :change-date 20080623 :comments sentential-conjunction-cleanup)     
     )
    ((LF-PARENT ONT::POS-CONDITION)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date 20080623 :comments sentential-conjunction-cleanup)     
     )
    ((LF-PARENT ONT::POS-CONDITION)
     (TEMPL binary-constraint-gerund-templ)
     (example "be careful if walking")
     (meta-data :origin beetle2 :entry-date 20070609 :change-date 20080623 :comments sentential-conjunction-cleanup)     
     )    
    )
   )
   
  ((w::if w::so)
   (SENSES 
    ((TEMPL DISC-PRE-TEMPL)
     (LF-PARENT ONT::POS-CONDITION))
    )))
 )


(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  ((W::if W::BUT)
   (wordfeats (W::conj +) (w::but +))
   (SENSES
    ((LF ONT::BUT)
     (non-hierarchy-lf t)
     (TEMPL SUBCAT-ANY-TEMPL)
     )
    )
   )
  
  ))
