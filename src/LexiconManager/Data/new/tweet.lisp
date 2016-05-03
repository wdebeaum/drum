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
     (TEMPL agent-neutral-optional-templ) 
     (EXAMPLE "He tweeted (a sentence)." "He tweeted about his vacation")
     )
    )
  )
))

