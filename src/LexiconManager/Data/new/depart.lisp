;;;;
;;;; W::depart
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::depart
     (wordfeats (W::morph (:forms (-vb) :nom W::departure)))
   (SENSES
    ((LF-PARENT ONT::DEPART)
     (example "depart from Rochester for NYC")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL GO-FROM-TO-TEMPL (xp1 (% W::PP (W::ptype W::from))) (xp2 (% W::PP (W::ptype W::for))))
     )
    ;; this transitive use is added for robustness -- apparently people say this
    ((LF-PARENT ONT::DEPART)
     (meta-data :origin ralf :entry-date 20040930 :change-date nil :comments ralf.txt)
     (example "the truck departed atlanta for rochester")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL GO-FROM-TO-TEMPL (xp2 (% W::PP (W::ptype W::for))))
     (preference .9)
     )
    )
   )
))

