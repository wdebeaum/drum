;;;;
;;;; W::awake
;;;;

(define-words :pos W::v 
 :words (
   (W::awake
   (wordfeats (W::morph (:forms (-vb) :past W::awoke :pastpart w::awoken :ing W::awaking)))
   (SENSES
    ((EXAMPLE "he awoke at dawn")
     (LF-PARENT ONT::BODILY-PROCESS)
     (TEMPL agent-TEMPL)
     )
    ((EXAMPLE "he awoke happy")
     (LF-PARENT ONT::BODILY-PROCESS)
     (SEM (F::trajectory -))
     (TEMPL agent-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::awake
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080828 :change-date nil :comments nil)
     (LF-PARENT ONT::body-property-VAL)
     (example "he is asleep")
     )
    )
   )
))

