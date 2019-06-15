;;;;
;;;; W::snub
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::snub
   (wordfeats (W::morph (:forms (-vb) :nom w::snub)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090507 :comments nil :vn ("judgement-33") :wn ("snub%2:32:00" "snub%2:32:01"))
     (LF-PARENT ONT::reject)
     (TEMPL AGENT-AGENT1-NP-TEMPL) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090507 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::reject)
     (example "he snubbed it")
     (TEMPL AGENT-FORMAL-XP-TEMPL) 
     )
    )
   )
))

