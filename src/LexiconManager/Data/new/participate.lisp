;;;;
;;;; W::PARTICIPATE
;;;;


(define-words :pos W::V :templ AGENT-neutral-XP-TEMPL
 :words (
  (W::participate
   (wordfeats (W::morph (:forms (-vb) :past w::participated :ing W::participating)))
   (SENSES
    ((LF-PARENT ONT::participate-attend)
     (EXAMPLE "He participated in the meeting." "She participates in these activities regularly.")
     (templ AGENT-neutral-XP-TEMPL (xp (% W::pp (W::ptype W::in))))
     (meta-data :wn ("participate%2:41:00"))
    )
    )
   )
))
