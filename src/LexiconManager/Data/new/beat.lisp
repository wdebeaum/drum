;;;;
;;;; w::beat
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BEAT
   (SENSES
    ((LF-PARENT ONT::BEAT)
     (example "the first beat of the measure")
     (meta-data :wn ("beat%1:10:00"))
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (w::beat
   (wordfeats (W::morph (:forms (-vb) :past W::beat :pastpart W::beaten :ing W::beating :nom w::beating)))
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     (LF-PARENT ont::rhythmic-motion)
     (TEMPL affected-templ)
     (example "his heart was beating fast")
     )
    ((meta-data :origin ptb :entry-date 20100526 :change-date nil :comments nil)
     (LF-PARENT ont::hitting)
     (TEMPL agent-affected-xp-templ)
     (example "he beat his dog")
     )
    )
   )
))

