;;;;
;;;; W::subsequent
;;;;

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::subsequent
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((LF-PARENT ONT::SEQUENCE-VAL)
     (meta-data :origin task-learning :entry-date 20051028 :change-date nil :wn ("subsequent%3:00:00") :comments nil)
     (EXAMPLE "it applies to all subsequent copies")
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::to))))
     )
    )
   )
))

