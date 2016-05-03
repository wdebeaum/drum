;;;;
;;;; W::imply
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::imply
   (wordfeats (W::morph (:forms (-vb) :nom W::implication)))
   (SENSES
    ((LF-PARENT ONT::correlation) ; like indicate
     (example "a cough implies a cold")
     (TEMPL neutral-neutral-xp-templ)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("indicate-78"))
     )
    ((LF-PARENT ONT::correlation) ; like indicate
     (example "This implies that the cat ate the mouse.")
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )

    )
   )
))

