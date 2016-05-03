;;;;
;;;; W::shouldnt
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::shouldnt
  (wordfeats (W::morph (:forms NIL)) (W::vform W::fut) (W::agr (? vf W::1s 2s w::3s W::1p w::2p w::3p)) (w::neg +))
   (SENSES
    ((LF-PARENT ONT::SHOULD)
     (lf-form w::should)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (example "it cant fail")
     (TEMPL AUX-MODAL-TEMPL)
     (SYNTAX (W::VFORM W::PRES) (w::neg +))
     )
    ((LF-PARENT ONT::SHOULD)
     (lf-form w::should)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (example "it cant")
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::PRES) (w::neg +) (W::changesem +))
     )
    )
   )
))

