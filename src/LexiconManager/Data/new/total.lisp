;;;;
;;;; W::TOTAL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::TOTAL
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("total%1:06:00"))
     (LF-PARENT ONT::NON-MEASURE-ORDERED-DOMAIN)
     (TEMPL OTHER-RELN-TEMPL)
     )
    ((LF-PARENT ONT::NON-MEASURE-ORDERED-DOMAIN)
     (meta-data :origin calo-ontology :entry-date 20060713 :change-date nil :wn ("total%1:06:00") :comments caloy3)
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :templ agent-neutral-xp-templ
 :words (
  (W::total
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("register-54.1") :wn ("total%2:42:00"))
     (LF-PARENT ONT::weigh)
 ; like weigh
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("register-54.1") :wn ("total%2:42:00"))
     (LF-PARENT ONT::weigh)
     (TEMPL neutral-theme-xp-templ) ; like weigh
     )
    
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
;   )
  (W::TOTAL
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("total%5:00:00:gross:00"))
     (LF-PARENT ONT::WHOLE-complete)
     )
    )
   )
))

