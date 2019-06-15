;;;;
;;;; W::focus
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::focus
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::foci))))
   (SENSES
    ((LF-PARENT ont::visual-sharpness-scale)
     (TEMPL OTHER-RELN-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :wn ("focus%1:07:01") :comments caloy3)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::focus
   (SENSES
    ((LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "focus on the problem")
     (TEMPL agent-neutral-xp-templ (xp (% W::pp (W::ptype W::on))))
     (meta-data :origin calo :entry-date 20050308 :change-date nil :comments computer-purchasing :vn ("focus-87"))
     )
    ((meta-data :origin calo :entry-date 20050308 :change-date 20090504 :comments projector-purchasing)
     (LF-PARENT ONT::visual-adjust)
     (example "focus the projector")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

