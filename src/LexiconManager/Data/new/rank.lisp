;;;;
;;;; W::rank
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::rank
    (wordfeats (W::morph (:forms (-vb) :nom w::ranking)))
   (SENSES
    ((LF-PARENT ONT::ordering)
     (TEMPL agent-neutral-xp-templ)
     (example "how is this model ranked?")
     (meta-data :origin calo :entry-date 20050509 :change-date 20090501 :comments projector-purchasing)
     )
    )
   )
))

