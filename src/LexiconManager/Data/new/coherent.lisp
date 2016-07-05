;;;;
;;;; W::COHERENT
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::COHERENT
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20061106 :comments nil :wn ("coherent%3:00:00") :comlex (ADJECTIVE))
     (example "a coherent story")
;     (LF-PARENT ONT::ACCEPTABILITY-VAL)
     (LF-PARENT ONT::consistent)
     (TEMPL central-adj-templ)
     )
    )
   )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::coherent
   (SENSES
    (;(LF-PARENT ONT::CONTINUOUS-VAL)
     (LF-PARENT ONT::consistent)
     (meta-data :origin task-learning :entry-date 20051028 :change-date nil :wn ("coherent%3:00:00") :comments nil)
     (EXAMPLE "the novel's coherent plot")
     )
    )
   )
))

