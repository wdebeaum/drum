;;;;
;;;; W::denude
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::denude
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090529 :comments nil :vn ("cheat-10.6") :wn ("denude%2:30:00"))
     (LF-PARENT ONT::destroy-part-of-whole)
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::PP (W::ptype W::of)))) ; like rid
     )
    )
   )
))

