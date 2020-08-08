;;;;
;;;; W::incredible
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::incredible
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
      (LF-PARENT ONT::great-VAL)
     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::hi))
     (example "They did some incredible work today.")
     )
    ((LF-PARENT ONT::not-credible-val)
     (example "incredible tales of her exotic heritage")
     (meta-data :origin jhwang :entry-date 20170317 :change-date nil :comments nil)
     )
    )
   )
))

