;;;;
;;;; W::splatter
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::splatter
   (wordfeats (W::morph (:forms (-vb) :past W::splattered :ing W::splattering :nom w::splatter)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1") :wn ("splatter%2:35:01"))
     (LF-PARENT ONT::apply-on-surface)
 ; like spray
     )
    ((meta-data :origin caet :entry-date 20110114 :change-date nil :comments nil :vn ("spray-9.7-1"))
     ;(LF-PARENT ONT::cause-cover)
     (LF-PARENT ONT::apply-on-surface)
     (example "the paint splattered on the wall")
     (templ affected-templ)
     )
    )
   )
))

