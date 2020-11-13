;;;;
;;;; W::cover
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
(W::cover
   (SENSES
    (
     (lf-parent ont::covering)
     (meta-data :origin caet :entry-date 20111220)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
 (W::cover
   (wordfeats (W::morph (:forms (-vb) :past W::covered :ing w::covering)))
   (SENSES
    ((LF-PARENT ONT::cause-cover)
     ;(TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::PP (W::ptype (? pt W::with W::in)))))
     ;(TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     ;(SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "cover the table with the tablecloth")
     (meta-data :origin fruitcarts :entry-date 20060215 :change-date 20090911 :comments nil :vn ("contiguous_location-47.8") :wn ("cover%2:35:01"))
     )
    ((LF-PARENT ONT::cover)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     ;(SEM (F::Aspect F::stage-level))
     (example "the tablecloth covers/ is covering the table")
     (meta-data :origin trips :entry-date 20090911 :change-date nil :comments nil :vn ("contiguous_location-47.8") :wn ("cover%2:35:01"))
     )
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s11)
     ;;(LF-PARENT ONT::managing)
      ;(lf-parent  ont::manage) ;; 20120521 GUM change new parent
     (lf-parent  ont::taking-care-of)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "cover the situation")
     (preference 0.98)
     )
    )
   )
))

