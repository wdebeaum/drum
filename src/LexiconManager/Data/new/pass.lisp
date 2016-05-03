;;;;
;;;; W::pass
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::pass
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("send-11.1-1"))
     (LF-PARENT ONT::send)
     (example "pass me the salt")
     (TEMPL agent-affected-recipient-alternation-templ)
     ) 
    
    ;;;; when the truck passes
    ((LF-PARENT ONT::PASS-BY)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-optional-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::PASS W::by)
   (SENSES
        ;;;; when he truck passes by
    ((LF-PARENT ONT::PASS-BY)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-optional-TEMPL)
     )
    )
   )
))

