;;;;
;;;; W::delimit
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::delimit ; stative?
    (wordfeats (W::morph (:forms (-vb) :past W::delimited :ing W::delimiting :nom w::delimitation)))
   (SENSES
    ((meta-data :origin chf :entry-date 20070817 :change-date nil :comments nil)
     ;(LF-PARENT ONT::hindering)
     (LF-PARENT ONT::classify)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     )
    )
   )
))

