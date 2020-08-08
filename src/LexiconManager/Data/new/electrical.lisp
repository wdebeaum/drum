;;;;
;;;; W::ELECTRICAL
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::ELECTRICAL
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin unknown :entry-date 20060824 :change-date 20090915  :wn ("electrical%3:01:01"))
     ; 20111017 changed to new word-for-type ont::electrical for obtw demo
 ;    (LF-PARENT ONT::substantial-property-val)
     (lf-parent ont::electrical) 
     (example "electrical wire" "electrical state" "electrical machine")     
     (SEM (F::GRADABILITY -))
     )
    )
   )
))

