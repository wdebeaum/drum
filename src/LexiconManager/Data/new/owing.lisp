;;;;
;;;; w::owing
;;;;

#|
(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::owing
   (SENSES    
    ((LF-PARENT ONT::due-to)
     (TEMPL binary-constraint-s-or-NP-templ (xp (% w::pp (w::ptype w::to))))
     (Example "changes owing to a damaged bulb")
     (meta-data :origin beetle :entry-date 20081111 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::due-to)
     (TEMPL BINARY-CONSTRAINT-PRED-TEMPL (xp (% w::pp (w::ptype w::to))))
     (Example "The car is brown owing to rust")
     (meta-data :origin beetle :entry-date 20081111 :change-date nil :comments nil)
     )
    ))
))
|#

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::owing w::to)
   (SENSES    
    ((LF-PARENT ONT::due-to)
     (TEMPL binary-constraint-s-or-NP-templ )
     (Example "changes owing to a damaged bulb")
     (preference 1.0)
     )
    ))))
