;;;;
;;;; W::behave
;;;;

(define-words :pos W::V 
 :words (
  (W::behave
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("masquerade-29.6-1"))
     ;;(LF-PARENT ONT::acting)
     (lf-parent ont::act-behave) ;; 20120524 GUM change new parent
     (templ agent-templ)  ;; the like modification is a function of the adverbial
     )
    )
   )
))

