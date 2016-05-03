;;;;
;;;; W::criticize
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::criticize
    (wordfeats (W::morph (:forms (-vb) :nom w::criticism)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("judgement-33") :wn ("criticize%2:32:00" "criticize%2:33:00"))
     (LF-PARENT ONT::criticize)
     (TEMPL agent-addressee-templ) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090506 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::criticize)
     (example "he criticized it")
     (TEMPL agent-theme-xp-templ) 
     )
    )
   )
))

