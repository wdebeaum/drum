;;;;
;;;; W::excruciating
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::excruciating
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "an excruciating pain")
     (LF-PARENT ONT::severity-VAL)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::hi))
     (TEMPL central-adj-templ)
     )
    ))
))

