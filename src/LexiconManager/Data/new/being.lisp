;;;;
;;;; W::being
;;;;

(define-words :pos W::n :templ count-pred-templ
 :tags (:base500)
 :words (
	   (W::being
	  (SENSES
	   ((LF-PARENT ONT::organism)
	    (example "an extraterrestrial being")
	    (meta-data :origin gloss :entry-date 20100520 :change-date nil :comments nil :wn nil)
	    )
	   ))
))

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::BEING
   (wordfeats (W::morph (:forms NIL)) (W::vform W::ing))
   (SENSES
    ;;;; The truck is being loaded.
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (TEMPL PASSIVE-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; she's being the doctor.
    ;;;; swift 01/12/01 added aspect feature F_Stage-Level
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (SEM (F::Aspect F::Stage-Level))
     (LF-FORM W::be)
     (TEMPL neutral-pred-xp-templ)
     )
    ;;;; .. there is a box
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (TEMPL THERE-THEME-TEMPL)
     )
    )
   )
))

