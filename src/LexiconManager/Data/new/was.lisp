;;;;
;;;; W::WAS
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::WAS
   (wordfeats (W::morph (:forms NIL)) (W::vform W::past) (W::agr (? ag W::1s W::3s)))
   (SENSES
    ;;;; I was loading a truck
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL PROG-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; He was
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL AUX-NOCOMP-TEMPL)
     (SYNTAX (W::auxname W::progr) (W::changesem +))
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )

    ;;  He was to arrive at 5
    ((LF-PARENT ONT::EXPECTATION)
     (LF-FORM W::be)
     (TEMPL neutral-theme-subjcontrol-templ)
     (preference .98) ;; slighly disprefered to favor passive constructions over adjectives
     )
    ;;;; The truck was loaded.
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (TEMPL PASSIVE-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; I was hungry
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (TEMPL neutral-pred-xp-templ)
     )
    ;;;; I was the leader
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL neutral-neutral-equal-templ)
     )

     (  ;; the fact was he's happy
     (lf-parent ont::proposition-equal) 
     (LF-FORM W::be)
     (TEMPL propositional-equal-templ)
     )

   ;;;; .. there is a box
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (TEMPL THERE-THEME-TEMPL (xp (% w::NP (w::agr (? ag w::1s w::3s)))))
     (preference .98)
     )
    )
   )
))

