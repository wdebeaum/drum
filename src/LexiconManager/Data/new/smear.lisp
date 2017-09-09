;;;;
;;;; W::smear
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::smear
   (wordfeats (W::morph (:forms (-vb) :nom w::smear)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1") :wn ("smear%2:35:03"))
     (LF-PARENT ONT::apply-on-surface)
     (example "he smeared the paint on the wall")
 ; like spray
     )
    ((meta-data :origin caet :entry-date 20110114 :change-date nil :comments nil :vn ("spray-9.7-1"))
     ;(LF-PARENT ONT::cause-cover)
     (LF-PARENT ONT::apply-on-surface)
     (example "the paint smeared on the wall")
     (templ affected-templ)
     )
    )
   )
))

