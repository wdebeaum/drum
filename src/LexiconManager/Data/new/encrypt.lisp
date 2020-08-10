;;;;
;;;; W::encrypt
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::encrypt
   (wordfeats (W::morph (:forms (-vb) :nom w::encryption)))
   (SENSES
    ((LF-PARENT ONT::encode) 
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "he encrypted the message")
     )
    )
   )
))

