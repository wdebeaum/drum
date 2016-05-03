;;;;
;;;; W::going
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::going W::to)
   (wordfeats (W::morph (:forms NIL)) (W::vform W::ing))
   (SENSES
    ((LF-PARENT ONT::GOING-TO)
;     (TEMPL GONNA-TEMPL)
     (TEMPL AUX-MODAL-TEMPL)
     (PREFERENCE 1.07) ;; boosted b.c. of multi-word processing?
     )
    )
   )
))

