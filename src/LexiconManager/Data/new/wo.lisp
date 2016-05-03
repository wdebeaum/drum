;;;;
;;;; W::WO
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::WO
   (wordfeats (w::contraction +))    
   (SENSES
    ;;;; I will drive a truck
    ((LF-PARENT ONT::FUTURE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::will)
     (TEMPL AUX-FUTURE-TEMPL)
     (SYNTAX (W::VFORM W::FUT))
     )
    ((LF-PARENT ONT::FUTURE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::will)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::FUT) (W::changesem -))
     )
    )
   )
))

