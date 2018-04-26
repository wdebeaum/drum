;;;;
;;;; W::organize
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::organize
   (wordfeats (W::morph (:forms (-vb) :nom w::organization)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("create-26.4") :wn ("organize%2:31:00"))
     ;(LF-PARENT ONT::host) ;; GUM change new parent 20121027
     (LF-PARENT ONT::organizing)
     (example "She organized the robbery")
     )

    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("create-26.4-1"))
     (LF-PARENT ONT::arranging)
     (TEMPL agent-affected-xp-templ) ; like rearrange
     )
    
    )
   )
))

