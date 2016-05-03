;;;;
;;;; W::MILD
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::MILD
   (wordfeats (W::morph (:FORMS (-LY -er))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("mild%3:00:00"))
     (LF-PARENT ONT::SEVERITY-VAL)
     (sem (f::gradability +) (f::intensity ont::lo) (f::orientation ont::less))
     (TEMPL LESS-ADJ-TEMPL)
     )    
    )
   )
))

