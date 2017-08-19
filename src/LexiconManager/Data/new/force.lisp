;;;;
;;;; w::force
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
	 (w::force
	  (senses ((lf-parent ont::strength-scale) 
                   (EXAMPLE "he hit with all the force he could muster")
		   (templ other-reln-templ)
		   (meta-data :origin bee :entry-date 20040407 :change-date nil :comments test-s)		   
		   )
		  ((lf-parent ont::physical-phenomenon) 
		   (example "the force of the hurricane")
		   (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil))
		  ))
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
  (W::force
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He forced him [to run for office]")  
     )
    ((meta-data :origin mobius :entry-date 20080826 :change-date nil :comments nil)
     (LF-PARENT ont::push)
     (example "the fuel was forced into the cylinder")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

