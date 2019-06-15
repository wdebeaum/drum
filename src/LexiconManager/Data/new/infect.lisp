;;;;
;;;; W::infect
;;;;

(define-words :pos W::v 
 :words (
  (W::infect
   (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ( ;;(LF-PARENT ONT::affect)
     (meta-data :origin beetle :entry-date 20081107 :change-date nil :comments nil)
     (LF-PARENT ont::objective-influence)
     (EXAMPLE "it infected him")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     )
    )
   )
))
