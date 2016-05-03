;;;;
;;;; W::WERENT
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :words (
(W::WERENT
   (wordfeats (W::morph (:forms NIL)) (W::vform W::past) (W::agr (? vf W::2s w::2p w::1p w::3p)) (w::neg +))
   (SENSES
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL PROG-TEMPL)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     )
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL AUX-NOCOMP-TEMPL)
     (SYNTAX (W::auxname W::progr) (W::changesem +))
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
      )
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (TEMPL PASSIVE-TEMPL)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     )
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (TEMPL neutral-pred-xp-templ)
     )
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL neutral-theme-xp-templ)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (TEMPL THERE-THEME-TEMPL (xp (% w::NP (w::agr (? ag w::3p w::2s w::2p w::1p)))))
     )
    )
   )
))

