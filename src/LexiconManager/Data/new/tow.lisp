;;;;
;;;; W::TOW
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::TOW w::TRUCK)
   (SENSES
    ((meta-data :origin obtw :entry-date 20111006)
     ;; 20111006 added for obtw demo
     (LF-PARENT ONT::tow-truck)
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::tow
   ;; adding :nom for obtw demo
   (wordfeats (W::morph (:forms (-vb) :nom W::tow)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20110928 :comments nil :vn ("carry-11.4") :wn ("tow%2:35:00"))
     (LF-PARENT ONT::haul)
 ; like drag
     )
    )
   )
))

