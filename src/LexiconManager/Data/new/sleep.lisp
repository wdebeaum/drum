;;;;
;;;; W::sleep
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::sleep
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("nap%1:28:00"))
     (LF-PARENT ONT::bodily-process)
     (example "he's having a sleep")
     )
    )
   )
))

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::sleep
   (wordfeats (W::morph (:forms (-vb) :past W::slept :nom W::sleep)))
   (SENSES
    ((LF-PARENT ONT::bodily-process)
     (TEMPL agent-templ)
     )
    )
   )
))

