;;;;
;;;; W::COORDINATE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::COORDINATE
   (SENSES
    ((meta-data :origin lou :entry-date 20040412 :change-date nil :wn ("coordinate%1:09:00") :comments lou)
     (LF-PARENT ONT::coordinate)
     (TEMPL Coordinate-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::coordinate
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::coordinating)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::PP (W::ptype W::with))))
     (example "coordinate your travel details with him")
     )
    ;; need this template to go through the adjectival passive rule
     ((LF-PARENT ONT::coordinating)
     (example "coordinate the details")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

