;;;;
;;;; W::delimit
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
   (W::delimit
    (wordfeats (W::morph (:forms (-vb) :past W::delimited :ing W::delimiting :nom w::delimitation)))
   (SENSES
    ((meta-data :origin chf :entry-date 20070817 :change-date nil :comments nil)
     (LF-PARENT ONT::hindering)
     (TEMPL agent-affected-xp-templ) 
     )
    )
   )
))

