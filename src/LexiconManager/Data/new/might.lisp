;;;;
;;;; W::might
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::might
   (SENSES
    ;;;; I might drive a truck
    ((LF-PARENT ONT::POSSIBILITY)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::might)
     (TEMPL COND-PRES-TEMPL)
     )
    ;;;; I might have driven a truck
    ((LF-PARENT ONT::POSSIBILITY)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::might)
     (TEMPL COND-PAST-TEMPL)
     )
    ((LF-PARENT ONT::POSSIBILITY)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::might)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::PRES) (W::changesem +))
     )
    )
   )
))

