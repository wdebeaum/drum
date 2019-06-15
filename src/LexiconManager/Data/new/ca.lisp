;;;;
;;;; W::ca
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
   (W::ca
    (wordfeats (w::contraction +))    
   (SENSES
    ;;;; I can't drive a truck i.e., I CA N^T
    ((LF-PARENT ONT::ABILITY)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::CAN)
     (TEMPL AUX-MODAL-TEMPL)
     (SYNTAX (W::VFORM W::PRES))
     )
    ((LF-PARENT ONT::ABILITY)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::can)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::PRES) (W::changesem +))
     )
    )
   )
))

