;;;;
;;;; W::sunder
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::sunder
   (wordfeats (W::morph (:forms (-vb) :past W::sundered :ing W::sundering)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("disassemble-23.3") :wn ("sunder%2:30:00"))
     (LF-PARENT ONT::unattach)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::from)))) ; like disconnect
     )
    )
   )
))

