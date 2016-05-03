;;;;
;;;; W::WILL
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::WILL
   (SENSES
    ;;;; I will drive a truck
    ((LF-PARENT ONT::FUTURE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::will)
     (TEMPL AUX-FUTURE-TEMPL)
     (SYNTAX (W::VFORM W::FUT))
     )
    ((LF-PARENT ONT::FUTURE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::will)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::FUT) (W::changesem -))
     )
    )
   )
))

(define-words :pos W::V 
 :tags (:base500)
 :words (
	 (W::will
	  (SENSES
	   ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("future_having-13.3") :wn ("will%2:40:00"))
	    (LF-PARENT ONT::future-giving)
	    (TEMPL agent-affected-recipient-alternation-templ) ; like owe
	    ;; MD 2009/03/18 -- this preference has to be kept quite low or otherwise constructions like
	    ;; "I think they will light" parse with "will" as future-giving
	    ;; It should not matter in general because common forms like "willed" and "willing" could not be confused with an auxilliary
	    (preference 0.94)
	    )
	   )
	  )
))

