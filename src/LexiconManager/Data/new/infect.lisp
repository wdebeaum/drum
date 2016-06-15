;;;;
;;;; W::infect
;;;;

(define-words :pos W::v 
 :words (
  (W::infect
   (wordfeats (W::morph (:forms (-vb) :nom W::infection)))
   (SENSES
    ( ;;(LF-PARENT ONT::affect)
     (meta-data :origin beetle :entry-date 20081107 :change-date nil :comments nil)
     (LF-PARENT ont::objective-influence)
     (EXAMPLE "it infected him")
     (TEMPL agent-affected-xp-templ) 
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::infection
  (senses;;;;; names of diseases/conditions that can appear without an article
   ((meta-data :wn ("infection%1:26:00"))
    (LF-PARENT ONT::infection)
    (TEMPL bare-pred-TEMPL)
    )
   )
)
))


