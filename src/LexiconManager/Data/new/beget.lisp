;;;;
;;;; W::beget
;;;;

(define-words :pos W::V :templ agent-affected-create-templ
 :words (
  (W::beget
   (wordfeats (W::morph (:forms (-vb) :past W::begot :pastpart W::begotten :ing W::begetting)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("engender-27"))
     (LF-PARENT ONT::create)
 ; like create,generate
     )
    )
   )
))

