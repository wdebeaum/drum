;;;;
;;;; W::ARE
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::ARE
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pres) (W::agr (? vf W::2s W::1p W::2p W::3p)))
   (SENSES
    ;;;; I am loading a truck
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL PROG-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; you are
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL AUX-NOCOMP-TEMPL)
     (SYNTAX (W::auxname W::progr) (W::changesem +))
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
      ;;  you are to arrive at 5
    ((LF-PARENT ONT::EXPECTATION)
     (LF-FORM W::be)
     (TEMPL NEUTRAL-FORMAL-CP-SUBJCONTROL-TEMPL)
     (preference .98) ;; slighly disprefered to favor passive constructions over adjectives
     )
    ;;;; The truck was loaded.
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (TEMPL PASSIVE-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; You are in trouble
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (TEMPL NEUTRAL-FORMAL-PRED-SUBJCONTROL-TEMPL)
     (preference .98) ;; slighly disprefered to favor passive constructions over adjectives
     )
    ;;;; You are the winner
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-A-TEMPL)
    
     )
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-B-TEMPL)
     
     )

     (  ;; the fact is he's happy
     (lf-parent ont::proposition-equal) 
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-CP-STHAT-EQUAL-TEMPL)
     )

    ;;;; .. there is a box
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (TEMPL EXPLETIVE-NEUTRAL-XP-TEMPL (xp (% w::NP (w::agr (? ag w::3p w::2s w::2p w::1p)))))
     )
    )
   )
))

