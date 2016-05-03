;;;;
;;;; W::quiz
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::quiz
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080520 :change-date nil :comments nil)
     (LF-PARENT ONT::gathering-event)
     (TEMPL OTHER-RELN-THEME-TEMPL)
     (example "he passed all his quizes")
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::quiz
   (wordfeats (W::morph (:forms (-vb) :past W::quizzed :ing W::quizzing :nom w::quiz)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("investigate-35.4") :wn ("quiz%2:32:00"))
     (LF-PARENT ONT::questioning)
     (TEMPL  AGENT-ADDRESSEE-TEMPL)
     )
    )
   )
))

