;;;;
;;;; W::must
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::must
   (SENSES
    ;;;; I must drive a truck
    ((LF-PARENT ONT::must)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::must)
     (TEMPL COND-PRES-TEMPL)
     )
    ;;;; I must have driven a truck
    ((LF-PARENT ONT::must)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::must)
     (TEMPL COND-PAST-TEMPL)
     )
    ((LF-PARENT ONT::must)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::must)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     )
    )
   )
))

