;;;;
;;;; W::predict
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::predict
     (wordfeats (W::morph (:forms (-vb) :past W::predicted :nom w::prediction)))
   (SENSES
    ((LF-PARENT ONT::predict)
;     (SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
      (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil :wn ("predict%2:32:00"))
     (example "he predicts that it will rain tomorrow")
     )
      )
   )))

