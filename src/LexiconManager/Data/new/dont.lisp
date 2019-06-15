;;;;
;;;; W::dont
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::dont
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pres) (W::agr (? vf W::1s 2s W::1p w::2p w::3p)) (w::neg +))
   (SENSES
    ((LF-PARENT ONT::DO)
     (lf-form w::do)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (TEMPL AUX-MODAL-TEMPL)
     )
    ((LF-PARENT ONT::DO)
     (lf-form w::do)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     )
    )
   )
))

