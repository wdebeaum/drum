;;;;
;;;; W::CAPACITY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CAPACITY
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::able-scale)
     ;(TEMPL OTHER-RELN-templ)
     (templ other-reln-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )  
))

