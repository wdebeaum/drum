;;;;
;;;; W::tailor
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::tailor
    (wordfeats (W::morph (:forms (-vb) :past W::tailored :ing W::tailoring)))
   (SENSES
    ((EXAMPLE "this message is custom-tailored for you")
     (LF-PARENT ONT::ACCOMMODATE)
     (TEMPL agent-affected2-TEMPL (xp (% W::pp (W::ptype (? pt W::for w::to)))))
     (meta-data :origin task-learning :entry-date 200500909 :change-date nil :comments nil)
     )
    ))
))

