;;;;
;;;; W::wouldnt
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::wouldnt
  (wordfeats (W::morph (:forms NIL)) (W::vform W::fut) (W::agr (? vf W::1s 2s w::3s W::1p w::2p w::3p)) (w::neg +))
   (SENSES
    ((LF-PARENT ONT::FUTURE)
     (lf-form w::will)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (TEMPL AUX-MODAL-TEMPL)
     )
    ((LF-PARENT ONT::FUTURE)
     (lf-form w::will)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     )
    )
   )
))

