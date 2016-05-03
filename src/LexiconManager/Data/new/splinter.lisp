;;;;
;;;; W::splinter
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::splinter
   (wordfeats (W::morph (:forms (-vb) :past W::splintered :ing W::splintering)))
   (SENSES
    #||((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("splinter%2:30:00" "splinter%2:41:01"))
     (LF-PARENT ont::break-object)
 ; like tear
     )||#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("splinter%2:30:00" "splinter%2:41:01"))
     (LF-PARENT ont::break-object)
     (TEMPL agent-affected-xp-templ) ; like break
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("splinter%2:30:00" "splinter%2:41:01"))
     (LF-PARENT ont::break-object)
     (TEMPL affected-templ) ; like crash,tear,break
     )
        )
   )
))

