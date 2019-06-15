;;;;
;;;; W::shift
;;;;
#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::shift
   (SENSES
    ((meta-data :origin caloy2 :entry-date 20050418 :change-date nil :wn ("shift%1:11:00") :comments projector-purchasing)
     (LF-PARENT ONT::adjust)
     (example "a shift in perspective")
     (templ other-reln-theme-templ  (xp (% W::pp (W::ptype (? pt w::in)))))
     )
    )
   )
))|#

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::shift
    (wordfeats (W::morph (:forms (-vb) :nom w::shift)))
   (SENSES
    ((LF-PARENT ONT::adjust)
     (meta-data :origin calo-ontology :entry-date 20060106 :change-date nil :comments meeting-understanding)
     (example "shift the schedule")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

