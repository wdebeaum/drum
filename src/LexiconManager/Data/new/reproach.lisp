;;;;
;;;; W::reproach
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::reproach
   (wordfeats (W::morph (:forms (-vb) :nom w::reproach)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090507 :comments nil :vn ("judgement-33") :wn ("reproach%2:32:00"))
     (LF-PARENT ont::reprimand)
     (TEMPL agent-addressee-templ) ; like thank
     )
    )
   )
))

