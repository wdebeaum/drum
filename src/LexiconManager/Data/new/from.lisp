;;;;
;;;; W::FROM
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::FROM
   (SENSES
    ;; requires ont::val to be physical object and ont::of to be trajectory +
    ((LF-PARENT ONT::FROM-LOC)
     (example "they departed from the station")
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     )
    ;; a generalized sense of ont::from
    ((LF-PARENT ONT::FROM)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (meta-data :origin gloss :entry-date 20100520 :change-date nil :comments nil)
     (example  "to go from a waking to a sleeping state")
     (preference .97) ;; prefer from-loc if applicable
     )
    
    ((LF-PARENT ONT::original-material)
     (example "make it from stone")
     (meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    
    ;; for nontrajectory nouns
    ((LF-PARENT ONT::source-as-loc)
     (example "the train from atlanta" "the book from the library")
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (preference .98) 
     )
    ;; requires ont::val to be a time
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     (example "the meeting lasted from five to seven pm ")
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::FROM
   (SENSES
    ((LF (W::FROM))
     (non-hierarchy-lf t))
    )
   )
))

