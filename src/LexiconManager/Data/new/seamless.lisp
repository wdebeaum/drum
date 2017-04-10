;;;;
;;;; W::SEAMLESS
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::SEAMLESS
    (wordfeats (W::MORPH (:FORMS (-LY))))
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("seamless%3:00:00") :comments html-purchasing-corpus)
      (LF-PARENT ONT::smooth-val)
      (TEMPL central-ADJ-TEMPL)
      )
     )
    )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::seamless
   (SENSES
    (;(LF-PARENT ONT::CONTINUOUS-VAL)
     (LF-PARENT ONT::consistent)
     (meta-data :origin task-learning :entry-date 20051028 :change-date nil :wn ("seamless%5:00:00:coherent:00") :comments nil)
     (EXAMPLE "the novel's seamless plot")
     )
    )
   )
))

