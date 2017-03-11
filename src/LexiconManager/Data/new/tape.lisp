;;;;
;;;; w::tape
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::tape
   (SENSES
    ((meta-data :origin plow :entry-date 20050404 :change-date nil :wn ("tape%1:06:01") :comments nil)
     (LF-PARENT ont::data-storage-medium)
     (example "do they carry books on tape")
     )
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("tape%1:06:00") :comments Office)
     (LF-PARENT ONT::material)
     (example "use a piece of tape to fasten the box")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
  (W::tape
   (SENSES
    ((EXAMPLE "tape this conversation john")
     (LF-PARENT ONT::RECORD)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments Break-contact :vn ("transcribe-25.4") :wn ("tape%2:32:00" "tape%2:32:03"))
     )
    ((LF-PARENT ONT::ATTACH)
     (example "tape the message to the window")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
      (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments Break-contact)
     )
    )
   )
))

