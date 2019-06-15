;;;;
;;;; W::survive
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::survive
   (wordfeats (W::morph (:forms (-vb) :nom w::survival :nomsubjpreps (w::of w::by) :nomobjpreps -)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("exist-47.1-1"))
     ;(LF-PARENT ONT::survive)
     (LF-PARENT ONT::live)
     (TEMPL affected-templ) ; like live
     )
    (;(LF-PARENT ONT::enduring)
     (LF-PARENT ONT::live)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL agent-neutral-optional-TEMPL)
     (TEMPL affected-neutral-xp-TEMPL)
     (example "he survived the experience")
     )
    )
   )
))

