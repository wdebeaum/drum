;;;;
;;;; W::acquit
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::acquit
   (wordfeats (W::morph (:forms (-vb) :past W::acquitted :ing W::acquitting)))
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090508 :comments nil :vn ("cheat-10.6-1-1"))
     (LF-PARENT ONT::pardon)
     (TEMPL AGENT-AFFECTED-FORMAL-XP-OPTIONAL-A-TEMPL (xp (% w::pp (w::ptype w::of)))) ; like pardon but different template
     )
    )
   )
))

