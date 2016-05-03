;;;;
;;;; W::glare
;;;;

(define-words :pos W::v 
 :words (
  (W::glare
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("nonverbal_expression-40.2") :wn ("glare%2:29:00"))
     (LF-PARENT ont::scrutiny)
     (TEMPL agent-templ)
     )
    ((EXAMPLE "glare at him")
     (LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-XP-TEMPL  (xp (% W::pp (W::ptype W::at))))
     (meta-data :origin calo :entry-date 20041026 :change-date nil :comments caloy2)
     )
    )
  )
  ))

