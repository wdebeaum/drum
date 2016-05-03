;;;;
;;;; W::occur
;;;;

(define-words :pos W::v 
 :words (
  (W::occur
   (wordfeats (W::morph (:forms (-vb) :nom w::occurrence)))
   (SENSES
    ((LF-PARENT ONT::occurring)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL neutral-TEMPL)
     (EXAMPLE "save error messages that occur in java applets")
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil :vn ("occurrence-48.3") :wn ("occur%2:30:00"))
     )
    )
   )
))

