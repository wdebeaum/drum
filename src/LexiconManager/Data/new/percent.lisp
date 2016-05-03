;;;;
;;;; W::percent
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::percent
    (wordfeats (W::morph (:forms (-s-3p) :plur W::percent)))
   (SENSES
    ((LF-PARENT ONT::percent)
     (meta-data :origin calo :entry-date 20041206 :change-date nil :wn ("percent%1:24:00") :comments caloy2)
     (example "this cpu is 20 percent faster")
     (TEMPL SUBSTANCE-UNIT-TEMPL)
     ;;(templ other-reln-theme-templ)
     )
    )
   )
))

