;;;;
;;;; W::slip
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::slip
   (SENSES
    ((LF-PARENT ONT::problem)
     (meta-data :origin lam :entry-date 20050422 :change-date nil :wn ("slip%1:04:05") :comments lam-initial)
     )
    ))
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::slip
   (wordfeats (W::morph (:forms (-vb) :nom w::slip)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("send-11.1-1"))
     (LF-PARENT ONT::cause-move) ;send)
     (TEMPL AGENT-AFFECTED-TEMPL) ; like mail,send,forward,transmit
     )
    )
   )
))

