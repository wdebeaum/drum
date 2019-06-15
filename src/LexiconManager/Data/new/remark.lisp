;;;;
;;;; W::remark
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::remark
   (SENSES
    ((LF-PARENT ONT::SAY)
     (example "the remark that he left")
     (templ count-subcat-that-optional-templ)
     (meta-data :origin task-learning :entry-date 20050926 :change-date nil :wn ("remark%1:10:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::remark
    (wordfeats (W::morph (:forms (-vb) :nom W::remark)))
   (SENSES
    ((LF-PARENT ONT::SAY)
     (meta-data :origin "verbnet-2.0" :entry-date 20060519 :change-date nil :comments nil :vn ("say-37.7-1" "lecture-37.11-1") :wn ("remark%2:32:00" "remark%2:32:01"))
     (example "He remarked that three teams are going to Delta")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-finite)))))
     )
    (
     (LF-PARENT  ONT::SAY)
     (meta-data :origin "verbnet-2.0" :entry-date 20060519 :change-date nil :comments nil :vn ("say-37.7-1" "lecture-37.11-1") :wn ("remark%2:32:00" "remark%2:32:01"))
     (example "he remarked about it [to her]")
     (TEMPL AGENT-FORMAL-AGENT1-2-XP1-PP-3-XP2-PP-WITH-OPTIONAL-TEMPL)
     )
    )
   )
))

