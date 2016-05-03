;;;;
;;;; W::judge
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::judge
   (wordfeats (W::morph (:forms (-vb) :nom w::judgement)))
   (SENSES
    ((LF-PARENT ONT::scrutiny)
     (example "he judged him")
     (TEMPL agent-neutral-xp-templ)
     )
    ((LF-PARENT ONT::believe)
     (example "he judged his opponent incompetent")
     (TEMPL experiencer-neutral-objcontrol-pred-templ)
     (meta-data :origin calo :entry-date 20060124 :change-date nil :comments meeting-understanding :vn ("declare-29.4-1-1-2"))
     )
    )
   )
))

