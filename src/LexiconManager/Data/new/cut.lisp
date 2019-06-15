;;;;
;;;; W::cut
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::cut
   (wordfeats (W::morph (:forms (-vb) :past W::cut :ing W::cutting :nom W::cut)))
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date 20090512 :comments nil :vn ("amuse-31.1"))
     (LF-PARENT ONT::evoke-hurt)
     (example "the criticism cut him deeply")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090504 :comments nil :vn ("build-26.1-1") :wn ("cut%2:36:00" "cut%2:36:11"))
     (LF-PARENT ONT::shape-change)
     (example "cut a dress" "cut paper dolls")
     (TEMPL AGENT-AFFECTED-RESULT-XP-NP-TEMPL (xp (% w::pp (w::ptype w::into)))) ; like carve
     (PREFERENCE 0.96)
     )
    (;;(LF-PARENT ONT::cut)
     (lf-parent ont::carve-cut) ;; 20120524 GUM change new parent
     (EXAMPLE "he cut the rope")
     )
    (
     (lf-parent ont::decrease) 
     (EXAMPLE "The government cut the subsidy.")
     (PREFERENCE 0.98)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 ((W::cut (W::off))
   (wordfeats (W::morph (:forms (-vb) :past W::cut :ing W::cutting :nom W::cut)))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20050831 :change-date nil :comments Break-contact)
     (LF-PARENT ONT::stop)
     (example "he cut off communications")
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::cut (W::up))
   (wordfeats (W::morph (:forms (-vb) :past W::cut :ing W::cutting)))
   (SENSES
    ((LF-PARENT ONT::cut)
     (LF-FORM W::cut-up)
     )
    )
   )
))

