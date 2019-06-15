;;;;
;;;; W::forbid
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::forbid
   (wordfeats (W::morph (:forms (-vb) :past W::forbade :pastpart W::forbidden :ing W::forbidding)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090528 :comments nil :vn ("forbid-67"))
     (LF-PARENT ONT::prohibit)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL)
     (example "I forbid you to see him")
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090603 :change-date nil :comments nil)
     (LF-PARENT ONT::prohibit)
     (TEMPL AGENT-AFFECTED-FORMAL-XP-PP-FROM-TEMPL  (xp (% w::cp (w::ctype w::s-from-ing))))
     (example "I forbid you from seeing him")
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090528 :change-date nil :comments nil)
     (LF-PARENT ONT::prohibit)
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
     (example "The hospital forbids smoking in the hallways")
     )
    )
   )
))

