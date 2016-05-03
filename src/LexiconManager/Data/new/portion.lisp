;;;;
;;;; W::portion
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::portion
   (SENSES
    ((LF-PARENT ONT::part)
     (TEMPL classifier-mass-templ)
     (meta-data :origin foodkb :entry-date 20050707 :change-date 20090605 :wn ("portion%1:13:00") :comment nil)
     (preference .96)
     )
    ((meta-data :origin fruitcarts :entry-date 20060816 :change-date nil :comments nil :wn ("part%1:09:00"))
     (LF-PARENT ONT::PART)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

