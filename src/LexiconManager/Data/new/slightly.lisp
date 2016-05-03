;;;;
;;;; w::slightly
;;;;

(define-words 
 :pos W::adv :templ pred-vp-templ
 :words  
 (
;; added for portability experiment
  (w::slightly
   (senses
    ((lf-parent ont::degree-modifier-low)
     (example "turn slightly")
     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
     )
    ((LF-PARENT ONT::DEGREE-MODIFIER-LOW)
     (meta-data :origin calo :entry-date 20050215 :change-date nil :comments caloy2)
     (example "there's a slightly quieter one but it weighs 5 pounds")
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     )
    ))
))

