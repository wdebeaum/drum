;;;;
;;;; W::nonaggressive
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::nonaggressive
    (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20081016 :change-date nil :comments nil)
     (LF-PARENT ONT::boldness-val)
     (example "nonagressive person")
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin calo :entry-date 20081016 :change-date nil :comments nil)
     (LF-PARENT ONT::boldness-val)
     (example "nonaggressive strategy")
     (templ central-adj-content-templ)
     )
    )
   )
))

