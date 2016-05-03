;;;;
;;;; W::DEAD
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::DEAD
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("dead%3:00:01"))
     (LF-PARENT ONT::LIVING-VAL)
     )
    ((LF-PARENT ONT::PRECISE)
     (example "dead center")
     (TEMPL ATTRIBUTIVE-ONLY-ADJ-TEMPL)
     (meta-data :origin mobius :entry-date 20080826 :change-date 20090731 :comments engine-texts)
     )    
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::dead
   (SENSES
    ((LF-PARENT ONT::degree-modifier-high)
     (example "dead right" "dead simple")
     (TEMPL ADJ-OPERATOR-TEMPL)
     (meta-data :origin mobius :entry-date 20080826 :change-date nil :comments engine-texts)
     )
    )
   )
))

