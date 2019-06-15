;;;;
;;;; W::tweet
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::tweet
   (SENSES
    ((LF-PARENT ONT::text-representation)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::tweet
   (SENSES
    (
     (LF-PARENT ONT::nonverbal-say)
     (TEMPL AGENT-NEUTRAL-XP-OPTIONAL-TEMPL) 
     (EXAMPLE "He tweeted (a sentence)." "He tweeted about his vacation")
     )
    )
  )
))

