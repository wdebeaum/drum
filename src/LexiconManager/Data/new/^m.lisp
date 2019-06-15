;;;;
;;;; W::^M
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::^M
   (wordfeats (W::morph (:forms NIL)) 
	      (W::vform W::pres) (W::agr W::1s)
	      (w::contraction +)
	      )
   (SENSES
    ;;;; I am loading a truck
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL PROG-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; The truck was loaded.
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (TEMPL PASSIVE-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; I^m hungry
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (TEMPL NEUTRAL-FORMAL-PRED-SUBJCONTROL-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; I'm the winner
    (;(;;LF-PARENT ONT::IN-RELATION)
      (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-TEMPL)
    
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    )
   )
))

