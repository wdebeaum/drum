;;;;
;;;; W::shatter
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::shatter
   (wordfeats (W::morph (:forms (-vb) :past W::shattered :ing W::shattering)))
   (SENSES
    #||((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("shatter%2:30:00" "shatter%2:30:01"))
     (LF-PARENT ont::break-object)
 ; like tear
     )||#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("shatter%2:30:00" "shatter%2:30:01"))
     (LF-PARENT ont::break-object)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like break
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("shatter%2:30:00" "shatter%2:30:01"))
     (LF-PARENT ont::break-object)
     (TEMPL affected-templ) ; like crash,tear,break
     )
        
    )
   )
))

