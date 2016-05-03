;;;;
;;;; w::configure
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(w::configure
   (wordfeats (W::morph (:forms (-vb) :nom w::configuration)))
 (senses
  ((lf-parent ont::set-up-device)
   (example "configure the system")
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
   (TEMPL agent-theme-xp-templ)
   (meta-data :origin calo :entry-date 20040414 :change-date 20090504 :comments calo-y1v4)
   )
  )
 )
))

