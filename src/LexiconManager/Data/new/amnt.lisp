;;;;
;;;; W::AMNT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::AMNT
   (SENSES
    ((meta-data :origin trips :entry-date 2006080t3 :change-date nil :comments nil)
     (LF-PARENT ONT::quantity)
     (templ other-reln-mass-templ)
     ;(TEMPL indef-classifier-templ)
     ;; ont::of is mass or plural -- amount of cake/*person
     (example "a(n) (certain) amount of water" "a large amount of people")
     )
    )
   )
))

