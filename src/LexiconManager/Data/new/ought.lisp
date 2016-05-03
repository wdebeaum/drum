;;;;
;;;; W::ought
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::ought
   (wordfeats (W::morph (:forms (-vb) :3s W::ought)))
   (SENSES
    ((LF-PARENT ONT::NECESSITY)
     (example "he ought to go")
     (SEM (F::Aspect F::Indiv-level) (f::Time-span f::extended)) ;; don't allow temporal mods on the higher verb (need)
     (TEMPL neutral-theme-subjcontrol-templ)
     )
    )
   )
))

