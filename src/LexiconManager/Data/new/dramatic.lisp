;;;;
;;;; W::dramatic
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::dramatic
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((LF-PARENT ONT::severity-VAL)
     (example "dramatic improvements in performance")
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation ont::less))
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :wn ("dramatic%5:00:00:impressive:00") :comments nil)
     )
    )
   )
))

