;;;;
;;;; W::spatter
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::spatter
   (wordfeats (W::morph (:forms (-vb) :past W::spattered :ing W::spattering :nom w::spatter)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::filling)
 ; like spray
     )
    ((meta-data :origin caet :entry-date 20110114 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::filling)
     (example "the paint spattered on the wall")
     (templ affected-templ)
     )
    )
   )
))

