;;;;
;;;; W::could
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::could
   (SENSES
    ;;;; I could drive a truck
    ((LF-PARENT ONT::CONDITIONAL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::could)
     (TEMPL COND-PRES-TEMPL)
     )
    ;;;; I could have driven a truck
    ((LF-PARENT ONT::CONDITIONAL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::could)
     (TEMPL COND-PAST-TEMPL)
     )
    ((LF-PARENT ONT::CONDITIONAL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::could)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::PRES) (W::changesem +))
     )
    )
   )
))

