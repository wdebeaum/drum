;;;;
;;;; W::hear
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::hear
   (wordfeats (W::morph (:forms (-vb) :past W::heard)))
   (SENSES
    ;;;; I heard that you ...
    ((LF-PARENT ONT::becoming-aware)
     (SEM  (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ;;;; I heard the sound
    ((LF-PARENT ONT::ACTIVE-PERCEPTION)
     (TEMPL agent-NEUTRAL-TEMPL)
     )
    ;;;; I can't hear
    ((LF-PARENT ONT::active-perception)
     (SEM (F::Time-span F::extended))
     (TEMPL agent-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20090331 :change-date nil :comments missing-sense)
     (LF-PARENT ONT::active-perception)
     ;;(SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (example "he heard him arrive")
     (TEMPL experiencer-action-objcontrol-templ (xp (% W::VP (W::vform (? vf W::base w::ing)))))
     )
    )
   )
))

