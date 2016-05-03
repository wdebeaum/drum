;;;;
;;;; w::fish
;;;;

(define-words :pos W::n :templ MASS-PRED-TEMPL
 :tags (:base500)
 :words (
  (w::fish
   (wordfeats (W::morph (:forms (-S-3P) :plur w::fish)))
   (SENSES
    ((meta-data :origin food-kb :entry-date 20050801 :change-date nil :wn ("fish%1:13:00") :comments nil)
     (LF-PARENT ont::fish)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (w::fish
   (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    (
     (LF-PARENT ONT::PHYSICAL-ACTIVITY)
     (TEMPL agent-templ)
     )
    )
   )
))

