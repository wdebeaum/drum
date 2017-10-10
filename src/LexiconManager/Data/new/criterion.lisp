;;;;
;;;; W::criterion
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::criterion
   (wordfeats (W::morph (:forms (-S-3P) :plur W::criteria)))
   (SENSES
    ((LF-PARENT ONT::REQUIREMENTS)
     (EXAMPLE "the schools must comply with federal requirements")
     (TEMPL OTHER-RELN-TEMPL)
     (meta-data :origin calo :entry-date 20050425 :change-date nil :comments projector-purchasing)
     )
    ((LF-PARENT ONT::STANDARD)
     (EXAMPLE "we live by the standards/requirements of the community")
     (TEMPL OTHER-RELN-TEMPL)
     (meta-data :origin domain-reorganization :entry-date 20170821 :change-date nil)
     )
    )
   )
))

