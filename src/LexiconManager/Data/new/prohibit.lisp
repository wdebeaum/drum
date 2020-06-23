;;;;
;;;; W::prohibit
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::prohibit
   (wordfeats (W::morph (:forms (-vb) :past W::prohibited :ing W::prohibiting :nom prohibition)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090528 :comments nil :vn ("forbid-67"))
     (LF-PARENT ONT::prohibit)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "The hospital prohibits smoking")
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090528 :change-date nil :comments nil)
     (LF-PARENT ONT::prohibit)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL  (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
     (example "The hospital prohibits visitors from smoking")
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::prohibited
    (wordfeats (W::MORPH (:FORMS (-LY))))
   (SENSES
    ((lf-parent ont::not-permissible-val)
     )
    )
   )
))
