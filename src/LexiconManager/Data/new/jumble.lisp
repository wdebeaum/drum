;;;;
;;;; W::jumble
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::jumble
   (wordfeats (W::morph (:forms (-vb) :nom w::jumble)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("shake-22.3-2"))
     (LF-PARENT ONT::combine-objects)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::with)))) ; like combine
     )
    )
   )
))

