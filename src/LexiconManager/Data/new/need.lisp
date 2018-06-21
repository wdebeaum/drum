;;;;
;;;; W::NEED
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::NEED
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("need%1:26:00" "need%1:17:00"))
     (LF-PARENT ONT::REQUIREMENT-scale)
     (PREFERENCE 0.92) ;; prefer the verb sense
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::NEED
   (wordfeats (W::morph (:forms (-vb) :nom w::need)))
   (SENSES
    ((LF-PARENT ONT::want)
     (SEM (F::Aspect F::stage-level));; allow temporal modifiers here, as implicit /=action/
     (TEMPL experiencer-neutral-templ)
     (example "I need this book")
     )
    ((LF-PARENT ONT::want)
     (SEM (F::Aspect F::stage-level));; allow temporal modifiers here, as implicit /=action/
     (TEMPL experiencer-THEME-XP-TEMPL)
     (example "Assembly is needed")
     )
    ((LF-PARENT ONT::WANT)
     (SEM (F::Aspect F::indiv-level))
     (TEMPL experiencer-action-objcontrol-templ)
     (example "I need the truck to go")
     (PREFERENCE 0.98)   ;;;; prefer above reading first
     )
    ((LF-PARENT ONT::NECESSITY)
     (example "The town needs to have supplies")
     (SEM (F::Aspect F::Indiv-level)) ;; don't allow temporal mods on the higher verb (need)
     (TEMPL neutral-theme-subjcontrol-templ)
     )
    )
   )
))

