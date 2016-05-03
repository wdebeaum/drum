;;;;
;;;; W::alternatively
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::alternatively
   (SENSES
    ((LF-PARENT ONT::choice-option)
     (TEMPL DISC-TEMPL)
     )
    ((LF-PARENT ONT::choice-option)
     (TEMPL pred-s-vp-templ)
     )
    )
   )
))

