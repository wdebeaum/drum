;;;;
;;;; W::AMOUNT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::AMOUNT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("amount%1:03:00"))
     ;(LF-PARENT ONT::quantity)
     (LF-PARENT ONT::quantity-abstr)
     (templ classifier-mass-templ)
     ;;(templ other-reln-subcat-mass-templ)
     ;(templ other-reln-templ)
;     (TEMPL indef-classifier-templ)
     ;; ont::of is mass or plural -- amount of cake/*person
     (example "I drank a(n) (certain) amount of water")
     )
 
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("amount%1:03:00"))
     (LF-PARENT ONT::quantity-abstr)
     (templ other-reln-subcat-mass-templ)
     (example "The amount of water is large")
     )
 
    )
   )
))

