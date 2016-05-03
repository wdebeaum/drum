;;;;
;;;; W::bracket
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::bracket
   (SENSES
    ((meta-data :origin allison :entry-date 20041021 :change-date nil :wn ("bracket%1:14:00") :comments caloy2)
     (LF-PARENT ONT::NON-MEASURE-ORDERED-DOMAIN)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::bracket
   (wordfeats (W::morph (:forms (-vb) :past W::bracketed :ing W::bracketing)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("contiguous_location-47.8"))
     (LF-PARENT ONT::surround)
     (TEMPL neutral-neutral-xp-templ) ; like cover,surround
     )
    )
   )
))

