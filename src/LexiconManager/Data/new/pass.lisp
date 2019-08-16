;;;;
;;;; W::pass
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::pass
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("send-11.1-1"))
     (LF-PARENT ONT::send)
     (example "pass me the salt")
     (TEMPL AGENT-AFFECTED-TEMPL)
     ) 
    ((LF-PARENT ont::passing)
     (example "The teacher passed the student although he was weak")
     )
    
    ;;;; when the truck passes
    ((LF-PARENT ONT::PASS-BY)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-XP-OPTIONAL-TEMPL)
     )

    ((LF-PARENT ONT::TIME-ELAPSE)
     (comment "The winter passed")
     (TEMPL NEUTRAL-TEMPL)
     (preference .991)  ;; ever so slight preference (when subject is a time!)
     )
   
   ))
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::PASS W::by)
   (SENSES
        ;;;; when he truck passes by
    ((LF-PARENT ONT::PASS-BY)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-XP-OPTIONAL-TEMPL)
     )

     ((LF-PARENT ONT::TIME-ELAPSE)
     (TEMPL NEUTRAL-TEMPL)
     )
     )
   )
  ))

