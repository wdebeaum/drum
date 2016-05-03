;;;;
;;;; W::alone
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::alone
   (SENSES
    ((EXAMPLE "he is alone")
     (LF-PARENT ONT::singularity-VAL) 
     (meta-data :origin calo :entry-date 20040907 :change-date nil :wn ("alone%5:00:00:unsocial:00") :comments caloy2)
     (SYNTAX (W::atype W::predicative-only))
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::ALONE
   (SENSES
    ((LF-PARENT ONT::exclusive)
     (LF-FORM W::alone)
     (example "he sang alone")
     (meta-data :origin calo :entry-date 20040907 :change-date nil :comments caloy2)
     (TEMPL PRED-S-POST-TEMPL)
     )
    )
   )
))

