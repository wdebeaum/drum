;;;;
;;;; W::decimate
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::decimate
    (wordfeats (W::morph (:forms (-vb) :nom decimation)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("destroy-44") :wn ("decimate%2:30:00"))
     (LF-PARENT ONT::destroy)
 ; like waste
     (syntax (w::resultative +))
     )
    )
   )
))

