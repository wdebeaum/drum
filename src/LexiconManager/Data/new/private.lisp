;;;;
;;;; W::private
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::private
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050817 :change-date 20090731 :wn ("private%5:00:00:inward:00") :comments nil)
     (example "private thoughts")
     (LF-PARENT ONT::secret-val)
     )
    ((LF-PARENT ont::private)
     (example "private school")
     (meta-data :origin adjective reorganization :entry-date 20170317 :change-date nil :comments nil)
     )
    )
   )
))

