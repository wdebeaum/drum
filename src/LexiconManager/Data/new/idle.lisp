;;;;
;;;; W::idle
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::idle
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((LF-PARENT ONT::INACTIVE)
     (SEM (F::GRADABILITY +))
     (EXAMPLE "a yellow dot indicates the person is idle")
     (meta-data :origin task-learning :entry-date 20050826 :change-date 20090731 :wn ("idle%3:00:00") :comments nil)
     )
    )
   )
))

