;;;;
;;;; W::care
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
     ((W::care w::giver)
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090403 :change-date nil :comments nil)
     (LF-PARENT ONT::person)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::CARE
   (wordfeats (W::morph (:forms (-vb) :nom W::care)))
   (SENSES
    ((LF-PARENT ONT::care)
     (SEM (F::Aspect F::Stage-Level))
     (TEMPL experiencer-THEME-XP-TEMPL (xp (% W::PP (W::ptype (? p W::for W::about)))))
     (example "I don't care about it")
     )
    ((LF-PARENT ONT::care)
     (TEMPL experiencer-TEMPL)
     (example "I don't care")
     )
    )
   )
))

