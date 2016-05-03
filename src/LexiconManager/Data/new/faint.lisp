;;;;
;;;; w::faint
;;;;

(define-words :pos W::v 
 :words (
  (w::faint
    (SENSES
     ((LF-PARENT ONT::bodily-process) ;; not a process - need a new lf type
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (templ agent-templ)
     (example "he fainted")
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (W::faint
     (wordfeats (W::MORPH (:FORMS (-ER -LY)))) 
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     (LF-PARENT ONT::weak)
     (templ central-adj-templ)
     (sem (f::gradability +) (f::intensity ont::lo) (f::orientation ont::less))
     )
    )
   )
))

