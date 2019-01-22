;;;;
;;;; w::little
;;;; 


(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::LITTLE
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("little%3:00:01"))
     (lf-parent ont::small)
     (TEMPL LESS-ADJ-TEMPL)
     (example "that's a little cat")
     (SEM (F::GRADABILITY F::+) (f::orientation ont::less) (f::intensity ont::med))
     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::LITTLE
   (wordfeats (W::status ont::indefinite)(W::AGR W::3s))
   (SENSES
    ((LF ONT::little)
     (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
     )
    ((LF ONT::little)
     (non-hierarchy-lf t)(TEMPL quan-bare-TEMPL)
     )  
   ((LF ONT::little)
     (non-hierarchy-lf t)
     (example "there is little of the truck left")
     (TEMPL quan-sing-sing-TEMPL)
     )
   )
   )
))

