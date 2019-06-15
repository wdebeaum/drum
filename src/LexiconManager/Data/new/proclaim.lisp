;;;;
;;;; W::proclaim
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::proclaim
    (wordfeats (W::morph (:forms (-vb) :nom W::proclamation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("say-37.7-1"))
     ;;(LF-PARENT ONT::announce)
     (lf-parent ONT::declare-performative)
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-to)))) ; like claim
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("confess-37.10"))
     ;;(LF-PARENT ONT::announce)
     (lf-parent ONT::declare-performative)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype (? c w::s-that))))) ; like acknowledge
     )
#|
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date nil :comments nil :vn ("say-37.7") :wn ("proclaim%2:32:00"))
     ;;(LF-PARENT ONT::talk)
     (lf-parent ont::mention-claim)
      (TEMPL agent-theme-to-addressee-optional-templ)  ; like say but needs different template b.c. doesn't participate in alternation
     )
|#
    )
   )
))

