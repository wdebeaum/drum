;;;;
;;;; W::feed
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::feed
   (SENSES
    ((LF-PARENT ONT::feed)
     (example "show me the feeds")
     ;; changed to newly created ont::feed for AKRL
     (meta-data :origin obtw :entry-date 20110914 :change-date 20110926 :comments demo)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
(W::feed
    (wordfeats (W::morph (:forms (-vb) :past W::fed :pastpart W::fed)))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090406 :change-date nil :comments nil)
     (LF-PARENT ONT::consume)
     (example "he fed on bread")
     (SEM (F::ASPECT F::DYNAMIC))
     (templ agent-affected-xp-templ  (xp (% W::pp (W::ptype W::on))))
     )
    ((LF-PARENT ONT::feeding)
     (example "feed some bread to the dog")
;     (TEMPL agent-affected-goal-templ) ; like grant,offer
     (TEMPL AGENT-AFFECTED-AR-TO-TEMPL)
     )
    ((lf-parent ont::feeding)
;     (templ agent-affected-recipient-alternation-templ)
     ;(templ AGENT-RECIPIENT-affected-TEMPL)
     (templ AGENT-RECIPIENT-affected-OPTIONAL-TEMPL (xp (% W::NP)))
     (example "feed the dog (some bread)")
     )
    
    
    )
   )
))

