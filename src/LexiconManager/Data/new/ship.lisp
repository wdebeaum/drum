;;;;
;;;; W::SHIP
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::SHIP
;;   (wordfeats (W::MORPH (:forms (-S-3P -load))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("ship%1:06:00"))
     (LF-PARENT ONT::VEHICLE)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::SHIP
   (wordfeats (W::morph (:forms (-vb) :nom w::shipment)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("send-11.1-1"))
     (LF-PARENT ONT::TRANSPORT)
     (example "ship the cargo to Avon")
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::ship
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("send-11.1-1"))
     (LF-PARENT ONT::send)
     (TEMPL AGENT-AFFECTED-TEMPL) ; like mail,send,forward,transmit
     )
    )
   )
))

