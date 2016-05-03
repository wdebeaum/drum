;;;;
;;;; W::protect
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::protect
  (wordfeats (W::morph (:forms (-vb) :nom W::protection)))
   (SENSES
    ((meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2 :wn-sense (1) :vn ("defend-85"))
     (LF-PARENT ONT::protecting)
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (example "buy a warranty to protect your purchase")
     (templ agent-affected-xp-templ)
     )  
    )
   )
))

