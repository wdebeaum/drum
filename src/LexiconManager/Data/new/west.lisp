;;;;
;;;; w::west
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (w::west
   (SENSES
    ((meta-data :origin coordops :entry-date 20070516 :change-date nil :comments nil)
     (LF-PARENT ONT::cardinal-point)
     (example "turn right to west")
     (TEMPL other-reln-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::WEST
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("west%3:00:00"))
     (LF-PARENT ONT::WEST)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::WEST W::OF)
   (SENSES
    ((LF-PARENT ONT::west-reln)
     (LF-FORM W::WEST-of)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::WEST
   (SENSES
    ((LF-PARENT ONT::CARDINAL-DIRECTION)
     (TEMPL PRED-S-POST-TEMPL)
     )
    )
   )
))

