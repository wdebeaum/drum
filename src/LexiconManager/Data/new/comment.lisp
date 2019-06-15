;;;;
;;;; W::comment
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::comment
    (wordfeats (W::morph (:forms (-vb) :nom W::comment)))
   (SENSES
    ((LF-PARENT ONT::SAY)
     (meta-data :origin "verbnet-2.0" :entry-date 20060519 :change-date nil :comments nil :vn ("say-37.7-1" "lecture-37.11-1") :wn ("remark%2:32:00" "remark%2:32:01"))
     (example "He commented that three teams are going to Delta")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-finite)))))
     )
    
    )
   )
))

