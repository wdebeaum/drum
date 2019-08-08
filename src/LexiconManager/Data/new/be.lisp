;;;;
;;;; W::BE
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::BE
   (wordfeats (W::morph (:forms NIL)) (W::vform W::base) (W::agr ?agr))
   (SENSES
    ;;;; ...be loading a truck
    ((LF-PARENT ONT::PROGRESSIVE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::be)
     (TEMPL PROG-TEMPL)
     )
    ((LF-PARENT ONT::PROGRESSIVE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::be)
     (TEMPL AUX-NOCOMP-TEMPL)
     (SYNTAX (W::auxname W::progr) (W::changesem +))
     )
    ;;;; ... be loaded.
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL PASSIVE-TEMPL)
     )

    ;;;; .. be happy
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (TEMPL NEUTRAL-FORMAL-PRED-SUBJCONTROL-TEMPL)
     (preference .98) ;; slighly disprefered to favor passive constructions over adjectives
     )
    ;;;; .. be the best
    (
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-TEMPL)
     )

    (  ;; the fact is he's happy
     (LF-PARENT ont::proposition-equal)
     (LF-FORM w::BE)
     (TEMPL NEUTRAL-NEUTRAL1-CP-STHAT-EQUAL-TEMPL)
     (EXAMPLE "the fact is that we went to the store")
     )

       ;;;; .. there is a box
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (TEMPL EXPLETIVE-NEUTRAL-XP-TEMPL)
     (preference .98)
     )


    )
   )
))

