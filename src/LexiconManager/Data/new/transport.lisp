;;;;
;;;; W::TRANSPORT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::TRANSPORT
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("transport%1:04:00"))
     (LF-PARENT ONT::VEHICLE)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::TRANSPORT
    (wordfeats (W::morph (:forms (-vb) :nom w::transport)))
    ;;;; transport the cargo to Avon
    (senses
     ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("send-11.1") :wn ("transport%2:35:01" "transport%2:38:00" "transport%2:38:01"))
     (LF-PARENT ONT::TRANSPORT)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )

    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("transport%2:37:00"))
     (LF-PARENT ont::evoke-joy)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     (PREFERENCE 0.96)
     )

    )
   )))
  

