;;;;
;;;; W::liberate
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::liberate
   (wordfeats (W::morph (:forms (-vb) :nom w::liberation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("free-80"))
     (LF-PARENT ONT::releasing)
     (TEMPL agent-affected-source-templ (xp (% w::pp (w::ptype w::from)))) ; like discharge
     )
    )
   )
))

