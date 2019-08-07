;;;;
;;;; W::ALTHOUGH
;;;;

#||(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::ALTHOUGH
   (SENSES
    ((LF-PARENT ONT::Qualification )
     (TEMPL binary-constraint-s-decl-templ)
     (example "you are right although I don't think you understand what I'm saying")
     (meta-data :origin plow :entry-date 20050922 :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
    )
   )  
))||#

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  (W::although
   (wordfeats (W::conj +))
   (SENSES
    ((LF ONT::BUT-EXCEPTION)
     (non-hierarchy-lf t)
     (TEMPL SUBCAT-ANY-TEMPL)
     )
     ((LF-PARENT ONT::qualification)
     (TEMPL binary-constraint-s-decl-templ)
      )
    )
   )))
