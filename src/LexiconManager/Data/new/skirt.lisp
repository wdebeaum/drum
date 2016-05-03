;;;;
;;;; W::SKIRT
;;;;

(define-words :pos W::n
 :words (
  ((W::SKIRT W::STEAK)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  (w::skirt
  (senses
   ((LF-PARENT ONT::attire)
    (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::skirt
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8") :wn ("skirt%2:35:00"))
     (LF-PARENT ONT::surround)
     (TEMPL neutral-neutral-xp-templ) ; like cover,surround
     )
    )
   )
))

