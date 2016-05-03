;;;;
;;;; W::gonna
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::gonna
   (wordfeats (W::morph (:forms NIL)) (W::vform W::ing))
   (SENSES
    ((LF-PARENT ONT::GOING-TO)
;     (TEMPL GONNA-TEMPL)
     (TEMPL AUX-MODAL-TEMPL)
;     (PREFERENCE 1.03) why is this boosted?
     )
    )
   )
))

