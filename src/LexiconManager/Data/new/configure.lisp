;;;;
;;;; w::configure
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(w::configure
   (wordfeats (W::morph (:forms (-vb) :nom w::configuration)))
 (senses
  ((lf-parent ont::set-up-device)
   (example "configure the system")
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
   (TEMPL AGENT-FORMAL-XP-TEMPL)
   (meta-data :origin calo :entry-date 20040414 :change-date 20090504 :comments calo-y1v4)
   )
  )
 )
))

(define-words :pos W::n 
  :words (
   (W::configuration
   (SENSES
    (
     (lf-parent ont::arrangement-configuration)
     (example "A configuration of stones")
     (TEMPL pred-subcat-contents-templ)
     )
   ))
))
