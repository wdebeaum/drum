;;;;
;;;; w::co
;;;;

(define-words :pos W::adv 
 :words (
  (w::co-
   (SENSES
    ((LF-PARENT ONT::INCLUSIVE)
     (TEMPL PRED-VP-PRE-templ)
     (example "coexist")
     )
    ((LF-PARENT ONT::INCLUSIVE)
     (TEMPL ADJ-OPERATOR-TEMPL)
     (example "coexistent")
     )
    )
   )
))

(define-words :pos W::adj 
 :words (
  (w::co-
   (SENSES
    ((LF-PARENT ONT::INCLUSIVE)
     (TEMPL central-adj-templ)
     (example "They are cochairs of the meeting.")
     )    
    )
   )
))

