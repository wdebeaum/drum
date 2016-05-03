;;;;
;;;; W::cant
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :words (
;; negated auxiliaries with no apostrophe -- added for asma texting 2012/01/30
 (W::cant
  (wordfeats (W::morph (:forms NIL)) (W::vform W::pres) (W::agr (? vf W::1s 2s w::3s W::1p w::2p w::3p)) (w::neg +))
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

