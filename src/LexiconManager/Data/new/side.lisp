;;;;
;;;; W::SIDE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::SIDE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::object-dependent-location)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::side W::effect)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::side W::effects))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("side_effect%1:26:00"))
     (LF-PARENT ONT::caused-event)
     (TEMPL COUNT-PRED-TEMPL)
     )
    )
   )
))

