;;;;
;;;; W::prefer
;;;;

(define-words :pos W::v 
 :words (
  (W::prefer
   (wordfeats (W::morph (:forms (-vb) :past W::preferred :nom w::preference :nomobjpreps (w::for w::of))))
   (SENSES
   ((LF-PARENT ONT::want)
    (example "I prefer this one")
    (templ experiencer-neutral-templ)
    )
   ((LF-PARENT ONT::WANT)
    (TEMPL EXPERIENCER-FORMAL-SUBJCONTROL-TEMPL)
    (meta-data :origin task-learning :entry-date 20060208 :change-date nil :comments nil)
    (EXAMPLE "I prefer to go")
    )
   ((LF-PARENT ONT::WANT)
    (TEMPL EXPERIENCER-NEUTRAL-FORMAL-CP-OBJCONTROL-B-TEMPL)
    (meta-data :origin task-learning :entry-date 20060208 :change-date nil :comments nil)
    (EXAMPLE "I prefer him to go")
    )
   )
  )
))

