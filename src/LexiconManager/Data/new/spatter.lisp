;;;;
;;;; W::spatter
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::spatter
   (wordfeats (W::morph (:forms (-vb) :past W::spattered :ing W::spattering :nom w::spatter)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1") :wn ("spatter%2:35:00"))
     (LF-PARENT ONT::apply-on-surface)
 ; like spray
     )
    ((meta-data :origin caet :entry-date 20110114 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::cause-cover)
     (example "the paint spattered on the wall")
     (templ affected-templ)
     )
    )
   )
))

