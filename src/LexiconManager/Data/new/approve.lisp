;;;;
;;;; W::approve
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::approve
   (wordfeats (W::morph (:forms (-vb) :nom approval)))
   (SENSES
    ((EXAMPLE "approve the purchase")
     (LF-PARENT ONT::allow)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin calo :entry-date 20040220 :change-date nil :comments calo-y1script)
     )
    ((EXAMPLE "he approves of the purchase")
     (LF-PARENT ONT::praise)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-theme-xp-templ (xp (% w::pp (w::ptype w::of))))
     (meta-data :origin calo :entry-date 20040220 :change-date 20090508 :comments calo-y1script)
     )
    )
   )
))

