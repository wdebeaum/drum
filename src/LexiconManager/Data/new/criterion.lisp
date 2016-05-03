;;;;
;;;; W::criterion
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::criterion
   (wordfeats (W::morph (:forms (-S-3P) :plur W::criteria)))
   (SENSES
    ((LF-PARENT ONT::REQUIREMENTS)
     (TEMPL OTHER-RELN-TEMPL)
     (meta-data :origin calo :entry-date 20050425 :change-date nil :comments projector-purchasing)
     )
    )
   )
))

