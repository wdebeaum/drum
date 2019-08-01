;;;;
;;;; W::at
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ;; primarily negative polarity
  ;; positive polarity: "you can go anywhere at all", "yes anyone at all can come"
  ((W::at w::all)
   (SENSES
   ((LF-PARENT ONT::least-extent)
    (templ postpositive-adj-templ)
     (meta-data :origin cardiac :entry-date 20090120 :change-date nil :comments LM-vocab)
     (example "none at all" "no plan at all")
     )
   ;; it is not quiet at all
   ;; it is not at all quiet ->  not at all
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::at W::least)
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::MIN)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::at W::most)
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::MAX)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::AT
   (SENSES
    ((LF-PARENT ONT::time-clock-rel)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     )
    #|
     ((LF-PARENT ONT::time-clock-rel)
      (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
      (preference .97)
      )
    |#
    #||((LF-PARENT ONT::time-clock-rel)
     (TEMPL BINARY-CONSTRAINT-adj-postpos-TEMPL)
     (meta-data :origin cernl :entry-date 20110114 :change-date nil :comments hpi-acn-3)
     (example "the device placed at that time")
     (preference .98)
     )||#
    ((LF-PARENT ONT::situated-in);;time-clock-rel)      ;;I think the "sit-val" reading of TiME-CLOCK-REL actually be SITUATED-IN  JFA 1/10
     (TEMPL binary-constraint-S-or-NP-TEMPL);;binary-constraint-SIT-VAL-NP-TEMPL)
     (preference .98))
    
    ((LF-PARENT ONT::AT-LOC)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (preference .991) ; slightly prefer this even if there is a semantic violation for this (and all the other senses)
     )

    
    #|
    ((LF-PARENT ONT::spatial-LOC)
     (TEMPL BINARY-CONSTRAINT-OF-STATE-NP-TEMPL)
     (meta-data :origin beetle2 :entry-date 20082002 :change-date nil :comments pilot1)
     (example "state at terminal")
     (preference 0.98) ;; don't choose if other options are available
     )
    |#
    
    ((lf-parent ont::rate-rel)
     (templ binary-constraint-s-or-np-templ)
     (example "move forward at one meter per second" "find a hotel at 10 dollars a day")
     (meta-data :origin coordops :entry-date 20070515)
     )
    ;;  this is not needed any more - its covered by the reworked rate-rel
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
		  :words (
			  ((w::at w::risk)
			   (SENSES
			    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("likely%5:00:00:prospective:00") :comments html-purchasing-corpus)
			     (EXAMPLE "He is a likely candidate")
			     (lf-parent ont::at-risk-val)
			     (SEM (F::GRADABILITY F::+))
			     (TEMPL central-adj-xp-TEMPL (XP (% W::pp (W::ptype (? xx W::of w::for)))))
			     )))))

(define-words :pos W::ADV
 :words (
  ((W::at w::the W::moment)
   (SENSES
    ((LF-PARENT ONT::NOW)
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (TEMPL PRED-S-VP-templ)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::at w::present)
   (SENSES
    ((meta-data :origin cernl :entry-date 20110317 :change-date nil :comments nil)
     (LF-PARENT ONT::NOW)
     (LF-FORM W::NOW)
     (example "at present he is president / he is president at present")
     (TEMPL PRED-S-TEMPL)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::AT
   (SENSES
    ((LF (W::AT))
     (non-hierarchy-lf t))
    )
   )
))

