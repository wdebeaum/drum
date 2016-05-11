;;;;
;;;; w::co
;;;;

(define-words :pos W::adv 
 :words (
  (w::co-
   (SENSES
    ((LF-PARENT ONT::INCLUSIVE)
     (TEMPL V-PREFIX-templ)
     (example "coexist")
     )
    ((LF-PARENT ONT::INCLUSIVE)
     (TEMPL adj-operator-prefix-TEMPL)
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
     (TEMPL prefix-adj-templ)
     (example "They are cochairs of the meeting.")
     )    
    )
   )
))

