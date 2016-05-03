;;;;
;;;; W::SET
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::SET
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20090520 :comments nil)
     (LF-PARENT ONT::group-object)
     (example "a set of dishes")
     (TEMPL classifier-count-pl-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
((w::set (w::up))
 (wordfeats (W::morph (:forms (-vb) :past W::set :ing W::setting)))
 (senses
  ((lf-parent ont::set-up-device)
   (example "set up the system")
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
   (TEMPL agent-theme-xp-templ)
   (meta-data :origin calo :entry-date 20041206 :change-date 20090504 :comments caloy2)
   )
  )
 )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::SET
   (wordfeats (W::morph (:forms (-vb) :past W::set :ing W::setting)))
   (SENSES
    ((lf-parent ont::change-device-state)
     (TEMPL AGENT-affected-RESULT-TEMPL (xp (% w::pp (w::ptype w::to))))
     (Example "Set the knob [to VDC]")
     (meta-data :origin bee :entry-date 20040412 :change-date nil :comments test-s)    
     )    
    ))
))

