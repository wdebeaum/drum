;;;;
;;;; W::sender
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::sender
   (SENSES
    ((LF-PARENT ONT::communication-party)
     (templ other-reln-templ)
     (EXAMPLE "you can reply to the sender and all other recipients")
     (meta-data :origin task-learning :entry-date 20050816 :change-date nil :wn ("sender%1:18:00") :comments nil)
     )
    )
   )
))

