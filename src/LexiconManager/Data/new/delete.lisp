;;;;
;;;; W::delete
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::delete
     (wordfeats (W::morph (:forms (-vb) :nom w::deletion)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("remove-10.1") :wn ("delete%2:32:00" "delete%2:35:00"))
     (LF-PARENT ONT::omit)
     (example "delete this paragraph from the text")
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype w::from))))
     )
    
    ((LF-PARENT ONT::discard)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-TEMPL)
     (meta-data :origin plot :entry-date 20080610 :change-date nil :comments nil :vn ("remove-10.1") :wn ("delete%2:32:00" "delete%2:35:00"))
     (example "delete")
     (preference .96)
     )
    )
   )
))

