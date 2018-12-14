;;;;
;;;; W::weep
;;;;

(define-words :pos W::V 
 :words (
	  (W::weep
           (wordfeats (W::morph (:forms (-vb) :past W::wept :ing W::weeping)))
	   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("nonverbal_expression-40.2") :wn ("weep%2:29:00"))
     (LF-PARENT ont::sound-expression)
     (EXAMPLE "She wept loudly.")
     (TEMPL agent-templ) ; like laugh
     )
;	    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("weep%2:29:00"))
	;     (LF-PARENT ONT::bodily-process)
	;     (TEMPL agent-affected-xp-templ) ; like vomit
	;     )
	;    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("weep%2:29:00"))
	 ;    (LF-PARENT ONT::bodily-process)
	 ;    (TEMPL agent-templ) ; like bleed
	 ;    )

	   )
)))

