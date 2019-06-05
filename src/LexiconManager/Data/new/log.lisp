;;;;
;;;; w::log
;;;;

(define-words :pos w::N 
 :words (
;; Mathematical functions
	 (w::log
	  (senses
	   ((LF-parent ONT::Number-measure-domain) 
	    (templ other-reln-templ)
	    (meta-data :origin lam :entry-date 20050420 :change-date nil :comments lam-initial)
	    )
	   ((LF-parent ont::natural-object)
	    (templ count-pred-templ)
	    )
	   )
	 
	  )))

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
  (W::log
   (SENSES
    ((EXAMPLE "Start logging my bio sensors")
     (LF-PARENT ONT::RECORD)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

