;;;;
;;;; W::cleanse
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::cleanse
   (SENSES
    ((LF-PARENT ONT::clean)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "cleanse the room")
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3))
    #|| ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090601 :comments nil :vn ("cheat-10.6") :wn ("cleanse%2:30:05"))
     (LF-PARENT ONT::clean)
     (TEMPL agent-affected-theme-optional-templ (xp (% w::pp (w::ptype w::of))))
     )||#
    )
   )
  ))

