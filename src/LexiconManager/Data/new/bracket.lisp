;;;;
;;;; W::bracket
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::bracket
   (SENSES
    ((meta-data :origin allison :entry-date 20041021 :change-date nil :wn ("bracket%1:14:00") :comments caloy2)
     (LF-PARENT ONT::range)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::bracket
   (wordfeats (W::morph (:forms (-vb) :past W::bracketed :ing W::bracketing)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8"))
     (LF-PARENT ONT::surround)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL) ; like cover,surround
     )
    )
   )
))

