;;;;
;;;; W::SPLIT
;;;;

(define-words :pos W::n
 :words (
  ((W::SPLIT W::PEA)
  (senses
	   ((LF-PARENT ONT::BEANS-PEAS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::SPLIT W::PEA W::WITH W::HAM)
  (senses
	   ((LF-PARENT ONT::SOUP)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
	  (w::split
	   (senses
        
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("split%2:30:01" "split%2:35:00"))
     (LF-PARENT ont::break-object)
     (TEMPL affected-templ) ; like crash,tear,break
     (PREFERENCE 0.96)
     )
	  #||  ((lf-parent ont::cut)
	     (example "split the table cell")
	     (templ agent-affected-xp-templ)
	     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :comments nil)
	     )	 ||#   
	    ((meta-data :origin beetle :entry-date 20080716 :change-date nil :comments nil :vn ("separate-23.1-2"))
	     (LF-PARENT ont::break-object)
	     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	     (TEMPL agent-affected-SOURCE-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
	     (EXAMPLE "a knife splits the wood")
	     )
	    ))
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::split
   (SENSES
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :wn ("split%5:00:02:divided:00") :comments s3)
     (LF-PARENT ONT::PART-WHOLE-val)
     (SEM (F::GRADABILITY F::-))
     )
    )
   )
))

