;;;;
;;;; W::INJUR
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::INJUR ; misspelling
    (wordfeats (W::morph (:forms (-vb) :past W::injured :ing W::injuring)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date 20090512 :comments nil :vn ("hurt-40.8.3-2") :wn ("injure%2:29:00"))
     (LF-PARENT ONT::evoke-injury)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

