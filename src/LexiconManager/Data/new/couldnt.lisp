;;;;
;;;; W::couldnt
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::couldnt
  (wordfeats (W::morph (:forms NIL)) (W::vform W::past) (W::agr (? vf W::1s 2s w::3s W::1p w::2p w::3p)) (w::neg +))
   (SENSES
    ((LF-PARENT ONT::ABILITY)
     (lf-form w::can)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (example "it cant fail")
     (TEMPL AUX-MODAL-TEMPL)
     )
    ((LF-PARENT ONT::ABILITY)
     (lf-form w::can)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (example "it cant")
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     )
    )
   )
))

