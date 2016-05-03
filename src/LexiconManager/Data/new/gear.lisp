;;;;
;;;; W::gear
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::gear
   (SENSES
    ((meta-data :origin calo :entry-date 20050509 :change-date nil :wn ("gear%1:06:01") :comments projector-purchasing)
     (LF-PARENT ONT::Functional-phys-object)
     (example "I need the power conversion gear")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::GEAR
   (SENSES
    ((EXAMPLE "this projector is specifically geared for home theater")
     (LF-PARENT ONT::ACCOMMODATE)
     (TEMPL agent-affected-theme-TEMPL (xp (% W::pp (W::ptype (? pt W::for w::to)))))
     (meta-data :origin calo :entry-date 20050527 :change-date nil :comments projector-purchasing)
     )
    ))
))

