;;;;
;;;; W::SPEAR
;;;;

(define-words :pos W::n
 :words (
  (W::SPEAR
  (senses
	   ((LF-PARENT ONT::PRODUCE) ;; need a different category here
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::spear
    (wordfeats (W::morph (:forms (-vb) :nom w::spear)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("carve-21.2-2"))
     (LF-PARENT ONT::cut) ; like chop
     )
    )
   )
))

