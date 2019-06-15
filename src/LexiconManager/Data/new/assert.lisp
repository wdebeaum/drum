;;;;
;;;; W::assert
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::assert
   (wordfeats (W::morph (:forms (-vb) :nom w::assertion)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060605 :change-date 20090506 :comments nil :vn ("conjecture-29.5-2" "reflexive_appearance-48.1.2") :wn ("assert%2:32:00" "assert%2:32:01"))
     ;(LF-PARENT ONT::announce)
     (lf-parent ont::assert)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype (? c w::s-that))))) ; like acknowledge
     )
    ((meta-data :origin jr :entry-date 20060605 :comments gloss-variant)
     ;(LF-PARENT ONT::announce)
     (lf-parent ont::assert)
     (example "he asserted his independence")
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

