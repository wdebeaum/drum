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

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (w::beat
   (wordfeats (W::morph (:forms (-vb) :past W::beat :pastpart W::beaten :ing W::beating :nom w::beating)))
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     (LF-PARENT ont::rhythmic-motion)
     (TEMPL affected-templ)
     (example "his heart was beating fast")
     )
    ((example "he beat his dog")
     (LF-PARENT ont::hitting)
     (meta-data :origin ptb :entry-date 20100526 :change-date nil :comments nil :wn ("beat%2:35:01"))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )

    ((example "he beat her in the race")
     (LF-PARENT ont::win-compete)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )

    )
   )
))

