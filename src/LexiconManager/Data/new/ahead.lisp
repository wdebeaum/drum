;;;;
;;;; W::AHEAD
;;;;

(define-words :pos W::ADV
 :words (
  (W::AHEAD
   (SENSES
    ((LF-PARENT ONT::front-of)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL  (xp (% w::pp (w::ptype (? pt w::of)))))
     (SYNTAX (W::ALLOW-DELETED-COMP +))
     (EXAMPLE "there is a crater ahead of me")
     (meta-data :origin joust :entry-date 20091026 :change-date nil :comments nil)
     )
    )
   )
))

