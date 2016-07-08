;;;;
;;;; W::FAST
;;;;

(define-words :pos W::n
 :words (
  ((W::FAST W::FOOD)
  (senses
	   ((LF-PARENT ONT::FAST-FOOD)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::FAST
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("fast%3:00:01"))
     (LF-PARENT ONT::SPEEDY)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("fast%5:00:00:hurried:00"))
     (LF-PARENT ONT::SPEEDY)
     (example "a fast meeting")
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL 
 :tags (:base500)
 :words (
	 (W::fast
	  (SENSES
	   ((LF-PARENT ONT::manner)
	    (TEMPL PRED-S-POST-TEMPL)
	    (example "Browne works fast")
	    (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
	    )
	   )	  
	  )
))

