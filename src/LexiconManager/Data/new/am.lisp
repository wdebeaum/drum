;;;;
;;;; W::AM
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::AM
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pres) (W::agr W::1s))
   (SENSES
    ;;;; I am loading a truck
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL PROG-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; I am
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL AUX-NOCOMP-TEMPL)
     (SYNTAX (W::auxname W::progr) (W::changesem +))
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (TEMPL PASSIVE-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )

  ;;  I am to arrive at 5
    ((LF-PARENT ONT::EXPECTATION)
     (LF-FORM W::be)
     (TEMPL neutral-theme-subjcontrol-templ)
     (preference .98) ;; slighly disprefered to favor passive constructions over adjectives
     )
    ;;;; I am hungry
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (TEMPL neutral-pred-xp-templ) 
     (preference .98) ;; slighly disprefered to favor passive constructions over adjectives
     )
    ;;;; I am the best
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL neutral-neutral-equal-templ)
     
     )
    )
   )
))

(define-words :pos W::n :templ PRED-NO-FORM-TEMPL
 :tags (:base500)
 :words (
  (W::AM
   (SENSES
    ((LF-PARENT ONT::time-object)
     (SEM (F::time-function F::day-period))
     )
    )
   )
))

