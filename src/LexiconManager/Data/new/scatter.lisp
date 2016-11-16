;;;;
;;;; W::scatter
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::scatter
   (wordfeats (W::morph (:forms (-vb) :past W::scattered :ing W::scattering)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::cause-cover)
 ; like spray
     )
    ((meta-data :origin caet :entry-date 20110114 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::motion)
     (example "the seeds scattered in the wind")
     (templ affected-templ)
     )
    )
   )
))

