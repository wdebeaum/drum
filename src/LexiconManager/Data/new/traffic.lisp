;;;;
;;;; W::traffic
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 ;  )
  (W::traffic
   (SENSES
    ((LF-PARENT ONT::direct-information)
     (EXAMPLE "the firewall blocks some network traffic")
     (meta-data :origin task-learning :entry-date 20050824 :change-date nil :wn ("traffic%1:10:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::traffic
   (wordfeats (W::morph (:forms (-vb) :past w::trafficked :ing w::trafficking)))
   (SENSES
    ((LF-PARENT ONT::TRANSPORT)
     (EXAMPLE "protein trafficking")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))
