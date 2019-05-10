;;;;
;;;; W::inflame
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::inflame
     (wordfeats (W::morph (:forms (-vb) :nom w::inflammation)))
   (SENSES
;    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090511 :comments nil :vn ("amuse-31.1"))
;     (LF-PARENT ONT::evoke-anger)
;     (TEMPL agent-affected-xp-templ)
;     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil :vn ("amuse-31.1"))
     (LF-PARENT ONT::provoke)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
#|
  (W::inflammation
   (SENSES
    ((meta-data :wn ("inflammation%1:26:00"))
     (LF-PARENT ONT::inflammation)
     (TEMPL mass-pred-TEMPL)
     )
    )
   )
|#
))

