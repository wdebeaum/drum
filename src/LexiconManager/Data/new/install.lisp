;;;;
;;;; w::install
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(w::install
   (wordfeats (W::morph (:forms (-vb) :nom w::installation)))
 (senses
  ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("put-9.1-1"))
   (LF-PARENT ONT::put)
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like stow
     (PREFERENCE 0.96)
   )
  ((lf-parent ont::set-up-device)
   (example "install the system")
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
   (meta-data :origin calo :entry-date 20041122 :change-date 20090504 :comments caloy2)
   )
  )
 )
))

