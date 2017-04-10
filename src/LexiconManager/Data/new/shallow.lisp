;;;;
;;;; W::shallow
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::shallow
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090731 :comments nil)
     (LF-PARENT ONT::shallow-val)
     (TEMPL LESS-ADJ-TEMPL)
     (example "a shallow ditch")
     )
    ((meta-data :origin cardiac :entry-date 20090130 :change-date 20090731 :comments nil)
     (LF-PARENT ONT::weak)
     (TEMPL LESS-ADJ-TEMPL)
     (example "shallow breathing")
     )
    )
   )
))

