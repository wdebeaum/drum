;;;;
;;;; W::thorough
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::thorough
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050919 :change-date nil :wn ("thorough%5:00:00:careful:00") :comments nil)
     (LF-PARENT ONT::attention-VAL)
     (example "a thorough person")
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin calo :entry-date 20040920 :change-date nil :wn ("careful%3:00:00") :comments caloy2)
     (LF-PARENT ONT::attention-VAL)
     (example "a thorough workout")
     (templ central-adj-content-templ)
     )
    )
   )
))

