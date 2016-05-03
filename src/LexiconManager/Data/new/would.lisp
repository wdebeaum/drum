;;;;
;;;; W::would
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::would
   (SENSES
    ;;;; I would drive a truck
    ((LF-PARENT ONT::CONDITIONAL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::would)
     (TEMPL COND-PRES-TEMPL)
     )
    ;;;; I would have driven a truck
    ((LF-PARENT ONT::CONDITIONAL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::would)
     (TEMPL COND-PAST-TEMPL)
     )
    ((LF-PARENT ONT::CONDITIONAL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::would)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::PRES))
     )
    )
   )
))

