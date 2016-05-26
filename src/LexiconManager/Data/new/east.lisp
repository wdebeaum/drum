;;;;
;;;; w::east
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (w::east
   (SENSES
    ((meta-data :origin coordops :entry-date 20070516 :change-date nil :comments nil)
     (LF-PARENT ONT::cardinal-point)
     (example "turn right to east")
     (TEMPL other-reln-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::EAST
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("east%3:00:00"))
     (LF-PARENT ONT::EAST-reln)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::EAST W::OF)
   (SENSES
    ((LF-PARENT ONT::east-reln)
     (LF-FORM W::EAST-of)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::EAST
   (SENSES
    ((LF-PARENT ONT::CARDINAL-DIRECTION)
     (TEMPL PRED-S-POST-TEMPL)
     )
    )
   )
))

