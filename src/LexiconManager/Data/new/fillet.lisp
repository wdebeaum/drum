;;;;
;;;; W::FILLET
;;;;

(define-words :pos W::n
 :words (
  (W::FILLET
  (senses
	   ((LF-PARENT ONT::MEAT)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::fillet
   (wordfeats (W::morph (:forms (-vb) :past W::filleted :ing W::filleting)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("carve-21.2-1"))
     (LF-PARENT ONT::cut)
 ; like grate
     )
    )
   )
))

