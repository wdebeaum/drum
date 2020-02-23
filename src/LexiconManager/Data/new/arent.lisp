;;;;
;;;; W::ARENT
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::ARENT
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pres) (W::agr (? vf W::2s W::1p W::2p W::3p)) (w::neg +))
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
     (TEMPL NEUTRAL-FORMAL-PRED-SUBJCONTROL-TEMPL)
     )
    (;;(LF-PARENT ONT::IN-RELATION)
      (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-A-TEMPL)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
    
     )
    (;;(LF-PARENT ONT::IN-RELATION)
      (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-B-TEMPL)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
    
     )
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (meta-data :origin asma :entry-date 20120130 :change-date nil)
     (TEMPL EXPLETIVE-NEUTRAL-XP-TEMPL (xp (% w::NP (w::agr (? ag w::3p w::2s w::2p w::1p)))))
     )
    )
   )
))

