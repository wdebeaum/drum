;;;;
;;;; W::Were
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::Were
   (wordfeats (W::morph (:forms NIL)) (W::vform W::past) (W::agr (? ag W::2s W::1p W::2p W::3p)))
   (SENSES
    ;;;; You were loading a truck
    ((LF-PARENT ONT::PROGRESSIVE)
     (TEMPL PROG-TEMPL)
     (LF-FORM W::be)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; they were
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL AUX-NOCOMP-TEMPL)
     (SYNTAX (W::auxname W::progr) (W::changesem +))
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )

  ;;  They were to arrive at 5
    ((LF-PARENT ONT::EXPECTATION)
     (LF-FORM W::be)
     (TEMPL NEUTRAL-FORMAL-CP-SUBJCONTROL-TEMPL)
     (preference .98) ;; slighly disprefered to favor passive constructions over adjectives
     )
    ;;;; The trucks were loaded.
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (TEMPL PASSIVE-TEMPL)
     )
    ;;;; They were hungry
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (TEMPL NEUTRAL-FORMAL-PRED-SUBJCONTROL-TEMPL)
     (preference .98) ;; slighly disprefered to favor passive constructions over adjectives
     )
    ;;;; The trucks were the convoy.
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-TEMPL)
     )
    (  ;; the fact is he's happy
     (LF-PARENT ont::proposition-equal)
     (LF-FORM w::BE)
     (TEMPL NEUTRAL-NEUTRAL1-CP-STHAT-EQUAL-TEMPL)
     )

    (  ;; the plan is to eat the pizza
     (LF-PARENT ont::proposition-equal)
     (LF-FORM w::BE)
     (TEMPL goal-equal-templ)
     )
    
    ;;;; .. there is a box
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (TEMPL EXPLETIVE-NEUTRAL-XP-TEMPL (xp (% w::NP (w::agr (? ag  w::2s w::1p w::2p w::3p)))))
     )
    )
   )
))

