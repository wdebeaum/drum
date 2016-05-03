;;;;
;;;; W::applaud
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::applaud
     (wordfeats (W::morph (:forms (-vb) :nom W::applause)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("applaud%2:32:00"))
     (LF-PARENT ONT::praise)
     (TEMPL agent-addressee-templ) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::praise)
     (example "he applauded the proposal")
     (TEMPL agent-theme-xp-templ) 
     )
    )
   )
))

