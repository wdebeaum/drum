;;;;
;;;; W::coalesce
;;;;

(define-words :pos W::v 
 :words (
   (W::coalesce
   (SENSES
    ((LF-PARENT ONT::combine-objects)
     (example "the groups coalesced")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-NP-PLURAL-TEMPL)
     (meta-data :origin gloss-training :entry-date 20100217 :change-date nil :comments nil)
    )
    
    ((LF-PARENT ONT::combine-objects)
     (example "freedom coalesced with volition" "religious tensions coalesced with political and economy rivalry")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     ;(TEMPL AGENT-WITH-CO-AGENT-XP-TEMPL)
     (TEMPL AGENT-AGENT1-XP-PP-TEMPL)
     (meta-data :origin gloss-training :entry-date 20100217 :change-date nil :comments nil)
    )

    ((LF-PARENT ONT::combine-objects)
     (example "religion colaesces social groups")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     ;(TEMPL agent-affected-XP-TEMPL)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
    )

    ((LF-PARENT ONT::appear)
     (example "the mist colaesced" "a group coalesced")
     (TEMPL affected-TEMPL)
    )

    )
   )
))


